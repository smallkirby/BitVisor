/*
 * Copyright (c) 2007, 2008 University of Tsukuba
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the name of the University of Tsukuba nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include "asm.h"
#include "constants.h"
#include "convert.h"
#include "current.h"
#include "gmm_access.h"
#include "mm.h"
#include "mmioclr.h"
#include "panic.h"
#include "string.h"
#include "vt_ept.h"
#include "vt_main.h"
#include "vt_paging.h"
#include "vt_regs.h"

#define MAXNUM_OF_EPTBL	256
#define DEFNUM_OF_EPTBL	16
#define EPTE_READ	0x1
#define EPTE_READEXEC	0x5
#define EPTE_WRITE	0x2
#define EPTE_LARGE	0x80
#define EPTE_ATTR_MASK	0xFFF
#define EPTE_MT_SHIFT	3
#define EPT_LEVELS	4

/*
	Per-CPU EPT information.

	Intel's EPT specification is described in its [manual](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html) at page `Vol 3C 28`.

	NOTE: The name of page tables are different in Linux and Intel's manual. As of 4-level paging:
		in Linux, tables are called as PGD, PUD, PMD, and PTE,
		in Intel's manual, tables are called PML4T[47:39], PDPT(Page-directory-pointer table)[38:30], PD(Page Directory)[29:21], PT(Page Table)[20:12].

	NOTE: EPT Translation Mechanism
		All tables (EPT PML4, EPT EPT PDPT, EPT PD, EPT PT) consist of 512 8byte entries.
		An entry in each table is identified using bits of gphys addr specified above (each [47:39],[38:30],[29:21],and [20:12]).
		For the specs, refer to [](/memo/EPT.md).
*/
struct vt_ept {
	int cnt;	// # of EPT tables
	int cleared;	// entries are cleared due to lack of table spaces
	void *ncr3tbl;	// hvirt addr of PML4 table instances
	phys_t ncr3tbl_phys; //hphys addr of PML4 table instances
	void *tbl[MAXNUM_OF_EPTBL];	// all tables currently used
	phys_t tbl_phys[MAXNUM_OF_EPTBL]; // hvirt addr of `tbl`
	struct {
		int level;	// indicate to which level EPT walk is done.
				// level==0 means that EPT PTE is identified for cur.gphys, meaning that no more page-walk is needed for this gphys.
				// level==EPT_LEVELS means that none of entries are identified, meaning that identifying PML4E, PDPTE, PDE and PTE are required.
		phys_t gphys;	// currently referenced gphys addr
		u64 *entry[EPT_LEVELS];	// PML4E, PDPTE, PDE, and PTE values currently identified
	} cur;	// EPT information of currently referenced page
};

static bool vt_ept_extern_mapsearch (struct vcpu *p, phys_t start, phys_t end);

static bool
vt_ept_mmioclr_callback (void *data, phys_t start, phys_t end)
{
	struct vcpu *p = data;
	return vt_ept_extern_mapsearch (p, start, end);
}

/*
	Init EPT structures of the CPU
*/
void
vt_ept_init (void)
{
	struct vt_ept *ept;
	int i;

	ept = alloc (sizeof *ept);
	// Zero-clear PML4 table
	alloc_page (&ept->ncr3tbl, &ept->ncr3tbl_phys);
	memset (ept->ncr3tbl, 0, PAGESIZE);
	ept->cleared = 1;
	// Zero-clear references to all tables
	for (i = 0; i < MAXNUM_OF_EPTBL; i++)
		ept->tbl[i] = NULL;
	// Alloc and init pre-defined numbef of tables
	for (i = 0; i < DEFNUM_OF_EPTBL; i++)
		alloc_page (&ept->tbl[i], &ept->tbl_phys[i]);
	ept->cnt = 0;
	ept->cur.level = EPT_LEVELS;
	current->u.vt.ept = ept;
	// Set EPTP in VMCS as 4-leve lpaging
	asm_vmwrite64 (VMCS_EPT_POINTER, ept->ncr3tbl_phys |
		       VMCS_EPT_POINTER_EPT_WB | VMCS_EPT_PAGEWALK_LENGTH_4);
	mmioclr_register (current, vt_ept_mmioclr_callback);
}

/*
	Do page walk for @gphys and get PML4E, PDPTE, PDE, and PTE as possible.

	This func updates ept->cur values.
	This func does **NOT** allocate new EPT table, nor create new EPT entries.

	NOTE: this func at least resolves PML4E address, hence ept->cur.entries[3] is always fulfilled and ept->cur.level must be -le 3.
*/
static void
cur_move (struct vt_ept *ept, u64 gphys)
{
	u64 mask, *p, e;

	// Check how much previously referenced EPT and currently referenced EPT can be shared (in other words, `re-used`).
	mask = 0xFFFFFFFFFFFFF000ULL;
		// (ept->cur.level must be 0 or 1, depending on previously referenced EPT is 4K-page or 2MB-page.)
		// If cur.level == 1, EPT PTE is not used cuz the page is 2MB
	if (ept->cur.level > 0)
		mask <<= 9 * ept->cur.level;
		// increment cur.level til previously referenced EPT(ept->cur.gphys) is same with currently referenced EPT.
	while (ept->cur.level < EPT_LEVELS &&
	       (gphys & mask) != (ept->cur.gphys & mask)) {
		ept->cur.level++;
		// `9` is the bit width to index entries in each tables
		mask <<= 9;
	}
	ept->cur.gphys = gphys;
		// if cur.level is 0, it means that previously selected page is same with currently referenced page.
		// all of cur.entries can be re-used as it is.
	if (!ept->cur.level)
		return;

	if (ept->cur.level >= EPT_LEVELS) {
		// ept->cur.level == EPT_LEVELS means that now none of PML4E, PDPTE, PDE, and PTE are identified.
		// identify PML4E here.
		p = ept->ncr3tbl;
		p += (gphys >> (EPT_LEVELS * 9 + 3)) & 0x1FF;
		ept->cur.entry[EPT_LEVELS - 1] = p;
		ept->cur.level = EPT_LEVELS - 1;
	} else {
		// if EPT page walk for this gphys addr is **partially** done, go to that entry
		p = ept->cur.entry[ept->cur.level];
	}
	// do page walking as possible
	// cur.level becomes 0 if `@gphys` is in 4K page and EPT PTE is present.
	// cur.level becomes 1 if:
	//	- `@gphys` is in 4K page and PDE is present, but PTE is **not** present (walking is **not** complete).
	//	- `@gphys` is in 2M page and PDE is present (walking is complete).
	while (ept->cur.level > 0) {
		e = *p;
		// if the entry doesn't have EPTE_READ permission, it means the entry is not present
		// if the entry has EPTE_LARGE permission, it means 2M page is used and no more page-walk is needed
		// 	NOTE: strictly speaking, it must check all of [2:0] bits, cuz EPT structure must be regarded as present if any bits of PTE[2:0] is 1.
		if (!(e & EPTE_READ) || (e & EPTE_LARGE))
			break;
		// convert the value in the entry into hvirt addr, store the value in ept->cur.entry
		e &= ~PAGESIZE_MASK;
		e |= (gphys >> (9 * ept->cur.level)) & 0xFF8;
		p = (u64 *)phys_to_virt (e);
		ept->cur.level--;
		ept->cur.entry[ept->cur.level] = p;
	}
}

/*
	Allocate a page for EPT @i-th table

	NOTE: if the @i-th table is already allocated, this func does nothing.
*/
static void
ept_tbl_alloc (struct vt_ept *ept, int i)
{
	if (!ept->tbl[i])
		alloc_page (&ept->tbl[i], &ept->tbl_phys[i]);
}

/*
	Allocate page tables, resolve its values, and fill the values.

	@level: to which level table entries would be resolved
	@return: pointer to EPT entry of @level.
		if @level==0(4K page), returns addr of PTE, with its value **NOT** fulfilled.
		if @level==1(2M page), returns addr of PDE, with its value **NOT** fulfilled.
*/
static u64 *
cur_fill (struct vt_ept *ept, u64 gphys, int level)
{
	int l;
	u64 *p;

	// # of EPT tables reached its limit, clear all
	if (ept->cnt + ept->cur.level - level > MAXNUM_OF_EPTBL) {
		memset (ept->ncr3tbl, 0, PAGESIZE);
		ept->cleared = 1;
		ept->cnt = 0;
		vt_paging_flush_guest_tlb ();
		ept->cur.level = EPT_LEVELS - 1; // discard previously identified entries
	}
	l = ept->cur.level;
	// Fill til @level entries
	for (p = ept->cur.entry[l]; l > level; l--) {
		ept_tbl_alloc (ept, ept->cnt);
		*p = ept->tbl_phys[ept->cnt] | EPTE_READEXEC | EPTE_WRITE;
		p = ept->tbl[ept->cnt++];
		memset (p, 0, PAGESIZE);
		p += (gphys >> (9 * l + 3)) & 0x1FF;
	}
	return p;
}

static void
vt_ept_map_page_sub (struct vt_ept *ept, bool write, u64 gphys)
{
	bool fakerom;
	u64 hphys;
	u32 hattr;
	u64 *p;

	cur_move (ept, gphys);
	p = cur_fill (ept, gphys, 0);
	hphys = current->gmm.gp2hp (gphys, &fakerom) & ~PAGESIZE_MASK;
	if (fakerom && write)
		panic ("EPT: Writing to VMM memory.");
	hattr = (cache_get_gmtrr_type (gphys) << EPTE_MT_SHIFT) |
		EPTE_READEXEC | EPTE_WRITE;
	if (fakerom)
		hattr &= ~EPTE_WRITE;
	*p = hphys | hattr;
}

/*
	Try to map new 2MB page for the @gphys.

	@return: true if new 2MB page is **NOT** mapped. false if new 2MB page is mapped.

	NOTE: return value is a little bit counterintuitive.
*/
static bool
vt_ept_map_2mpage (struct vt_ept *ept, u64 gphys)
{
	u64 hphys;
	u32 hattr;
	u64 *p;

	cur_move (ept, gphys);
	// cur.level==0 means that this @gphys is in 4KB page, no need to continue
	if (!ept->cur.level)
		return true;
	// ???: assign (convert? allocate?) hphys addr to @gphys
	hphys = current->gmm.gp2hp_2m (gphys & ~PAGESIZE2M_MASK);
	if (hphys == GMM_GP2HP_2M_FAIL)
		return true;
	// ???
	if (!cache_gmtrr_type_equal (gphys & ~PAGESIZE2M_MASK,
				     PAGESIZE2M_MASK))
		return true;
	hattr = (cache_get_gmtrr_type (gphys & ~PAGESIZE2M_MASK) <<
		 EPTE_MT_SHIFT) | EPTE_READEXEC | EPTE_WRITE | EPTE_LARGE;
	// Fill PML4E, PDPTE for this @gphys conversion
	p = cur_fill (ept, gphys, 1);
	// Assign the value to PDE
	*p = hphys | hattr;
	return false;
}

/*
	Get to which level EPT page walk is done.

	NOTE: cur.level==0 means that page walk is coomplete,
		while cur.level==1 means page walk for 4KB is incomplete **or** page walk for 2MB is complete.
	NOTE: this func updates ept->cur.
*/
static int
vt_ept_level (struct vt_ept *ept, u64 gphys)
{
	cur_move (ept, gphys);
	return ept->cur.level;
}

static void
vt_ept_map_page_clear_cleared (struct vt_ept *ept)
{
	u32 n, nn;
	u64 base, len, size;
	phys_t next_phys;

	ept->cleared = 0;
	n = 0;
	for (nn = 1; nn; n = nn) {
		nn = current->gmm.getforcemap (n, &base, &len);
		if (!len)
			continue;
		len += base & PAGESIZE_MASK;
		base &= ~PAGESIZE_MASK;
		while (len > 0) {
			size = PAGESIZE;
			if (vt_ept_level (ept, base) > 0 &&
			    !mmio_range (base & ~PAGESIZE2M_MASK, PAGESIZE2M)
			    && !vt_ept_map_2mpage (ept, base))
				size = (base | PAGESIZE2M_MASK) + 1 - base;
			else if (!(next_phys = mmio_range (base, PAGESIZE)))
				vt_ept_map_page_sub (ept, true, base);
			else
				size = (next_phys - base + PAGESIZE - 1) &
					~PAGESIZE_MASK;
			if (size > len)
				size = len;
			base += size;
			len -= size;
		}
	}
	if (ept->cleared)
		panic ("%s: error", __func__);
}

static void
vt_ept_map_page (struct vt_ept *ept, bool write, u64 gphys)
{
	if (ept->cleared)
		vt_ept_map_page_clear_cleared (ept);
	vt_ept_map_page_sub (ept, write, gphys);
	if (ept->cleared)
		vt_ept_map_page_clear_cleared (ept);
}

/*
	Handle EPT violation.

	@write: whether this violation is caused by write access
	@gphys: gphys addr to which access this violation is caused
*/
void
vt_ept_violation (bool write, u64 gphys)
{
	struct vt_ept *ept;

	ept = current->u.vt.ept;
	mmio_lock ();
	if (vt_ept_level (ept, gphys) > 0 && // page walk is incomplete or 2MB walk is complete
	    !mmio_range (gphys & ~PAGESIZE2M_MASK, PAGESIZE2M) && // ???
	    !vt_ept_map_2mpage (ept, gphys)) // 2MB page was **newly** mapped for this violation
		;
	else if (!mmio_access_page (gphys, true))
		vt_ept_map_page (ept, write, gphys); // maps 4K page
	mmio_unlock ();
}

void
vt_ept_tlbflush (void)
{
}

void
vt_ept_updatecr3 (void)
{
	ulong cr3, cr4;
	u64 tmp64;

	vt_paging_flush_guest_tlb ();
	if (!current->u.vt.lma && current->u.vt.vr.pg) {
		asm_vmread (VMCS_CR4_READ_SHADOW, &cr4);
		if (cr4 & CR4_PAE_BIT) {
			asm_vmread (VMCS_GUEST_CR3, &cr3);
			cr3 &= 0xFFFFFFE0;
			read_gphys_q (cr3 + 0x0, &tmp64, 0);
			asm_vmwrite64 (VMCS_GUEST_PDPTE0, tmp64);
			read_gphys_q (cr3 + 0x8, &tmp64, 0);
			asm_vmwrite64 (VMCS_GUEST_PDPTE1, tmp64);
			read_gphys_q (cr3 + 0x10, &tmp64, 0);
			asm_vmwrite64 (VMCS_GUEST_PDPTE2, tmp64);
			read_gphys_q (cr3 + 0x18, &tmp64, 0);
			asm_vmwrite64 (VMCS_GUEST_PDPTE3, tmp64);
		}
	}
}

void
vt_ept_clear_all (void)
{
	struct vt_ept *ept;

	ept = current->u.vt.ept;
	memset (ept->ncr3tbl, 0, PAGESIZE);
	ept->cleared = 1;
	ept->cnt = 0;
	ept->cur.level = EPT_LEVELS;
	vt_paging_flush_guest_tlb ();
}

static bool
vt_ept_extern_mapsearch (struct vcpu *p, phys_t start, phys_t end)
{
	u64 *e, tmp1, tmp2, mask = p->pte_addr_mask;
	unsigned int cnt, i, j, n = 512;
	struct vt_ept *ept;

	ept = p->u.vt.ept;
	cnt = ept->cnt;
	for (i = 0; i < cnt; i++) {
		e = ept->tbl[i];
		for (j = 0; j < n; j++) {
			if (!(e[j] & EPTE_READ))
				continue;
			tmp1 = e[j] & mask;
			tmp2 = tmp1 | 07777;
			if (e[j] & EPTE_LARGE) {
				tmp1 &= ~07777777;
				tmp2 |= 07777777;
			}
			if (start <= tmp2 && tmp1 <= end) {
				if (p != current)
					return true;
				e[j] = 0;
			}
		}
	}
	return false;
}

void
vt_ept_map_1mb (void)
{
	ulong gphys;
	struct vt_ept *ept;

	ept = current->u.vt.ept;
	vt_ept_clear_all ();
	for (gphys = 0; gphys < 0x100000; gphys += PAGESIZE) {
		mmio_lock ();
		if (!mmio_access_page (gphys, false))
			vt_ept_map_page (ept, false, gphys);
		mmio_unlock ();
	}
}
