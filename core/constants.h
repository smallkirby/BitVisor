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

#ifndef _CORE_CONSTANTS_H
#define _CORE_CONSTANTS_H

#define CPUID_1				0x1
#define CPUID_1_EBX_NUMOFLP_MASK	0x00FF0000
#define CPUID_1_EBX_NUMOFLP_1		0x00010000
#define CPUID_1_ECX_VMX_BIT		0x20
#define CPUID_1_ECX_SMX_BIT		0x40
#define CPUID_1_ECX_PCID_BIT		0x20000
#define CPUID_1_ECX_X2APIC_BIT		0x200000
#define CPUID_1_ECX_OSXSAVE_BIT		0x8000000
#define CPUID_1_EDX_PSE_BIT		0x8
#define CPUID_1_EDX_TSC_BIT		0x10
#define CPUID_1_EDX_MSR_BIT		0x20
#define CPUID_1_EDX_PAE_BIT		0x40
#define CPUID_1_EDX_APIC_BIT		0x200
#define CPUID_1_EDX_SEP_BIT		0x800
#define CPUID_1_EDX_MTRR_BIT		0x1000
#define CPUID_1_EDX_PAT_BIT		0x10000
#define CPUID_4_EAX_NUMOFTHREADS_MASK	0x03FFC000
#define CPUID_4_EAX_NUMOFCORES_MASK	0xFC000000
#define CPUID_6				0x6
#define CPUID_6_EAX_HW_FEEDBACK		0x80000
#define CPUID_6_EDX_HW_FEEDBACK_SIZE_MASK	0xF00
#define CPUID_6_EDX_HW_FEEDBACK_SIZE_SHIFT	8
#define CPUID_7_EBX_TSCADJUSTMSR_BIT	0x2
#define CPUID_7_EBX_INVPCID_BIT		0x400
#define CPUID_7_EBX_PT_BIT		0x2000000
#define CPUID_7_ECX_OSPKE_BIT		0x10
#define CPUID_EXT_0			0x80000000
#define CPUID_EXT_1			0x80000001
#define CPUID_EXT_1_ECX_SVM_BIT		0x4
#define CPUID_EXT_1_EDX_PAGE1GB_BIT	0x4000000
#define CPUID_EXT_1_EDX_64_BIT		0x20000000
#define CPUID_EXT_7			0x80000007
#define CPUID_EXT_7_EDX_TSCINVARIANT_BIT	0x100
#define CPUID_EXT_8			0x80000008
#define CPUID_EXT_8_EAX_PHYSADDRSIZE_MASK	0xFF
#define CPUID_EXT_A			0x8000000A
#define CPUID_EXT_A_EDX_NP_BIT		0x1
#define CPUID_EXT_A_EDX_SVM_LOCK_BIT	0x4
#define CPUID_EXT_A_EDX_NRIP_SAVE_BIT	0x8
#define CPUID_EXT_A_EDX_FLUSH_BY_ASID_BIT	0x40
#define MSR_IA32_TIME_STAMP_COUNTER	0x10
#define MSR_IA32_APIC_BASE_MSR		0x1B
#define MSR_IA32_APIC_BASE_MSR_ENABLE_X2APIC_BIT	0x400
#define MSR_IA32_APIC_BASE_MSR_APIC_GLOBAL_ENABLE_BIT	0x800
#define MSR_IA32_APIC_BASE_MSR_APIC_BASE_MASK	0xFFFFFF000ULL
#define MSR_IA32_FEATURE_CONTROL	0x3A
#define MSR_IA32_FEATURE_CONTROL_LOCK_BIT	0x1
#define MSR_IA32_FEATURE_CONTROL_VMXON_BIT	0x4
#define MSR_IA32_TSC_ADJUST		0x3B
#define MSR_IA32_BIOS_UPDT_TRIG		0x79
#define MSR_IA32_BIOS_SIGN_ID		0x8B
#define MSR_IA32_MTRRCAP		0xFE
#define MSR_IA32_MTRRCAP_VCNT_MASK	0xFF
#define MSR_IA32_MTRRCAP_FIX_BIT	0x100
#define MSR_IA32_MTRRCAP_WC_BIT		0x400
#define MSR_IA32_SYSENTER_CS		0x174
#define MSR_IA32_SYSENTER_ESP		0x175
#define MSR_IA32_SYSENTER_EIP		0x176
#define MSR_IA32_DEBUGCTL		0x1D9
#define MSR_IA32_MTRR_PHYSBASE0		0x200
#define MSR_IA32_MTRR_PHYSBASE0_TYPE_MASK	0xFFULL
#define MSR_IA32_MTRR_PHYSBASE0_PHYSBASE_MASK	0xFFFFFFFFFF000ULL
#define MSR_IA32_MTRR_PHYSMASK0		0x201
#define MSR_IA32_MTRR_PHYSMASK0_V_BIT	0x800ULL
#define MSR_IA32_MTRR_PHYSMASK0_PHYSMASK_MASK	0xFFFFFFFFFF000ULL
#define MSR_IA32_MTRR_PHYSBASE1		0x202
#define MSR_IA32_MTRR_PHYSMASK1		0x203
#define MSR_IA32_MTRR_PHYSBASE2		0x204
#define MSR_IA32_MTRR_PHYSMASK2		0x205
#define MSR_IA32_MTRR_PHYSBASE3		0x206
#define MSR_IA32_MTRR_PHYSMASK3		0x207
#define MSR_IA32_MTRR_PHYSBASE4		0x208
#define MSR_IA32_MTRR_PHYSMASK4		0x209
#define MSR_IA32_MTRR_PHYSBASE5		0x20A
#define MSR_IA32_MTRR_PHYSMASK5		0x20B
#define MSR_IA32_MTRR_PHYSBASE6		0x20C
#define MSR_IA32_MTRR_PHYSMASK6		0x20D
#define MSR_IA32_MTRR_PHYSBASE7		0x20E
#define MSR_IA32_MTRR_PHYSMASK7		0x20F
#define MSR_IA32_MTRR_PHYSBASE8		0x210
#define MSR_IA32_MTRR_PHYSMASK8		0x211
#define MSR_IA32_MTRR_PHYSBASE9		0x212
#define MSR_IA32_MTRR_PHYSMASK9		0x213
#define MSR_IA32_MTRR_FIX64K_00000	0x250
#define MSR_IA32_MTRR_FIX16K_80000	0x258
#define MSR_IA32_MTRR_FIX16K_A0000	0x259
#define MSR_IA32_MTRR_FIX4K_C0000	0x268
#define MSR_IA32_MTRR_FIX4K_C8000	0x269
#define MSR_IA32_MTRR_FIX4K_D0000	0x26A
#define MSR_IA32_MTRR_FIX4K_D8000	0x26B
#define MSR_IA32_MTRR_FIX4K_E0000	0x26C
#define MSR_IA32_MTRR_FIX4K_E8000	0x26D
#define MSR_IA32_MTRR_FIX4K_F0000	0x26E
#define MSR_IA32_MTRR_FIX4K_F8000	0x26F
#define MSR_IA32_PAT			0x277
#define MSR_IA32_MTRR_DEF_TYPE		0x2FF
#define MSR_IA32_MTRR_DEF_TYPE_TYPE_MASK	0xFFULL
#define MSR_IA32_MTRR_DEF_TYPE_FE_BIT	0x400ULL
#define MSR_IA32_MTRR_DEF_TYPE_E_BIT	0x800ULL
#define MSR_IA32_PERF_GLOBAL_CTRL	0x38F
#define MSR_IA32_VMX_BASIC		0x480
#define MSR_IA32_VMX_BASIC_TRUE_CTLS_BIT	0x80000000000000ULL
#define MSR_IA32_VMX_PINBASED_CTLS	0x481
#define MSR_IA32_VMX_PROCBASED_CTLS	0x482
#define MSR_IA32_VMX_EXIT_CTLS		0x483
#define MSR_IA32_VMX_ENTRY_CTLS		0x484
#define MSR_IA32_VMX_MISC		0x485
#define MSR_IA32_VMX_MISC_PT_IN_VMX_BIT	0x4000
#define MSR_IA32_VMX_MISC_VMWRITE_ALL_BIT	0x20000000ULL
#define MSR_IA32_VMX_CR0_FIXED0		0x486
#define MSR_IA32_VMX_CR0_FIXED1		0x487
#define MSR_IA32_VMX_CR4_FIXED0		0x488
#define MSR_IA32_VMX_CR4_FIXED1		0x489
#define MSR_IA32_VMX_PROCBASED_CTLS2	0x48B
#define MSR_IA32_VMX_EPT_VPID_CAP	0x48C
#define MSR_IA32_VMX_EPT_VPID_CAP_PAGEWALK_LENGTH_4_BIT	0x40
#define MSR_IA32_VMX_EPT_VPID_CAP_EPTSTRUCT_WB_BIT	0x4000
#define MSR_IA32_VMX_EPT_VPID_CAP_2MPAGE_BIT	0x10000
#define MSR_IA32_VMX_EPT_VPID_CAP_INVEPT_BIT	0x100000
#define MSR_IA32_VMX_EPT_VPID_CAP_INVEPT_ALL_CONTEXT_BIT	0x4000000
#define MSR_IA32_VMX_EPT_VPID_CAP_INVVPID_BIT	0x100000000ULL
#define MSR_IA32_VMX_EPT_VPID_CAP_INVVPID_SINGLE_CONTEXT_BIT 0x20000000000ULL
#define MSR_IA32_VMX_TRUE_PROCBASED_CTLS	0x48E
#define MSR_IA32_RTIT_OUTPUT_BASE	0x560
#define MSR_IA32_RTIT_OUTPUT_MASK_PTRS	0x561
#define MSR_IA32_RTIT_CTL		0x570
#define MSR_IA32_RTIT_STATUS		0x571
#define MSR_IA32_RTIT_CR3_MATCH		0x572
#define MSR_IA32_RTIT_ADDR0_A		0x580
#define MSR_IA32_RTIT_ADDR0_B		0x581
#define MSR_IA32_RTIT_ADDR1_A		0x582
#define MSR_IA32_RTIT_ADDR1_B		0x583
#define MSR_IA32_RTIT_ADDR2_A		0x584
#define MSR_IA32_RTIT_ADDR2_B		0x585
#define MSR_IA32_RTIT_ADDR3_A		0x586
#define MSR_IA32_RTIT_ADDR3_B		0x587
#define MSR_IA32_X2APIC_APICID		0x802
#define MSR_IA32_X2APIC_EOI		0x80B
#define MSR_IA32_X2APIC_LDR		0x80D
#define MSR_IA32_X2APIC_SIVR		0x80F
#define MSR_IA32_X2APIC_ICR		0x830
#define MSR_IA32_X2APIC_SELF_IPI	0x83F
#define MSR_IA32_HW_FEEDBACK_PTR	0x17D0
#define MSR_IA32_HW_FEEDBACK_PTR_VALID_BIT	0x1
#define MSR_IA32_HW_FEEDBACK_CONFIG	0x17D1
#define MSR_IA32_EFER			0xC0000080
#define MSR_IA32_EFER_SCE_BIT		0x1
#define MSR_IA32_EFER_LME_BIT		0x100
#define MSR_IA32_EFER_LMA_BIT		0x400
#define MSR_IA32_EFER_NXE_BIT		0x800
#define MSR_IA32_EFER_SVME_BIT		0x1000
#define MSR_IA32_EFER_FFXSR_BIT		0x4000
#define MSR_IA32_STAR			0xC0000081
#define MSR_IA32_LSTAR			0xC0000082
#define MSR_AMD_CSTAR			0xC0000083
#define MSR_IA32_FMASK			0xC0000084
#define MSR_IA32_FS_BASE		0xC0000100
#define MSR_IA32_GS_BASE		0xC0000101
#define MSR_IA32_KERNEL_GS_BASE		0xC0000102
#define MSR_AMD_VM_CR			0xC0010114
#define MSR_AMD_VM_CR_R_INIT_BIT	0x2
#define MSR_AMD_VM_CR_DIS_A20M_BIT	0x4
#define MSR_AMD_VM_CR_LOCK_BIT		0x8
#define MSR_AMD_VM_CR_SVMDIS_BIT	0x10
#define MSR_AMD_SYSCFG			0xC0010010
#define MSR_AMD_SYSCFG_MTRRFIXDRAMEN_BIT	0x40000
#define MSR_AMD_SYSCFG_MTRRFIXDRAMMODEN_BIT	0x80000
#define MSR_AMD_SYSCFG_MTRRTOM2EN_BIT	0x200000
#define MSR_AMD_SYSCFG_TOM2FORCEMEMTYPEWB_BIT	0x400000
#define MSR_AMD_TOP_MEM2		0xC001001D
#define MSR_AMD_TOP_MEM2_ADDR_MASK	0xFFFFFFF800000ULL
#define MSR_AMD_VM_HSAVE_PA		0xC0010117

/* 16-Bit Control Field */
#define VMCS_VPID			0x0

/* 16-Bit Guest-State Fields */
#define VMCS_GUEST_ES_SEL		0x800
#define VMCS_GUEST_CS_SEL		0x802
#define VMCS_GUEST_SS_SEL		0x804
#define VMCS_GUEST_DS_SEL		0x806
#define VMCS_GUEST_FS_SEL		0x808
#define VMCS_GUEST_GS_SEL		0x80A
#define VMCS_GUEST_LDTR_SEL		0x80C
#define VMCS_GUEST_TR_SEL		0x80E

/* 16-Bit Host-State Fields */
#define VMCS_HOST_ES_SEL		0xC00
#define VMCS_HOST_CS_SEL		0xC02
#define VMCS_HOST_SS_SEL		0xC04
#define VMCS_HOST_DS_SEL		0xC06
#define VMCS_HOST_FS_SEL		0xC08
#define VMCS_HOST_GS_SEL		0xC0A
#define VMCS_HOST_TR_SEL		0xC0C

/* 64-Bit Control Fields */
#define VMCS_ADDR_IOBMP_A		0x2000
#define VMCS_ADDR_IOBMP_A_HIGH		0x2001
#define VMCS_ADDR_IOBMP_B		0x2002
#define VMCS_ADDR_IOBMP_B_HIGH		0x2003
#define VMCS_ADDR_MSRBMP		0x2004
#define VMCS_ADDR_MSRBMP_HIGH		0x2005
#define VMCS_VMEXIT_MSRSTORE_ADDR	0x2006
#define VMCS_VMEXIT_MSRSTORE_ADDR_HIGH	0x2007
#define VMCS_VMEXIT_MSRLOAD_ADDR	0x2008
#define VMCS_VMEXIT_MSRLOAD_ADDR_HIGH	0x2009
#define VMCS_VMENTRY_MSRLOAD_ADDR	0x200A
#define VMCS_VMENTRY_MSRLOAD_ADDR_HIGH	0x200B
#define VMCS_EXEC_VMCS_POINTER		0x200C
#define VMCS_EXEC_VMCS_POINTER_HIGH	0x200D
#define VMCS_TSC_OFFSET			0x2010
#define VMCS_TSC_OFFSET_HIGH		0x2011
#define VMCS_VIRT_APIC_PAGE_ADDR	0x2012
#define VMCS_VIRT_APIC_PAGE_ADDR_HIGH	0x2013
#define VMCS_EPT_POINTER		0x201A
#define VMCS_EPT_POINTER_HIGH		0x201B
#define VMCS_VMREAD_BMP_ADDR		0x2026
#define VMCS_VMREAD_BMP_ADDR_HIGH	0x2027
#define VMCS_VMWRITE_BMP_ADDR		0x2028
#define VMCS_VMWRITE_BMP_ADDR_HIGH	0x2029
#define VMCS_XSS_EXITING_BMP		0x202C
#define VMCS_XSS_EXITING_BMP_HIGH	0x202D

/* 64-Bit Read-Only Data Field */
#define VMCS_GUEST_PHYSICAL_ADDRESS	0x2400
#define VMCS_GUEST_PHYSICAL_ADDRESS_HIGH	0x2401

/* 64-Bit Guest-State Fields */
#define VMCS_VMCS_LINK_POINTER		0x2800
#define VMCS_VMCS_LINK_POINTER_HIGH	0x2801
#define VMCS_GUEST_IA32_DEBUGCTL	0x2802
#define VMCS_GUEST_IA32_DEBUGCTL_HIGH	0x2803
#define VMCS_GUEST_IA32_PAT		0x2804
#define VMCS_GUEST_IA32_PAT_HIGH	0x2805
#define VMCS_GUEST_IA32_EFER		0x2806
#define VMCS_GUEST_IA32_EFER_HIGH	0x2807
#define VMCS_GUEST_IA32_PERF_GLOBAL_CTRL	0x2808
#define VMCS_GUEST_IA32_PERF_GLOBAL_CTRL_HIGH	0x2809
#define VMCS_GUEST_PDPTE0		0x280A
#define VMCS_GUEST_PDPTE0_HIGH		0x280B
#define VMCS_GUEST_PDPTE1		0x280C
#define VMCS_GUEST_PDPTE1_HIGH		0x280D
#define VMCS_GUEST_PDPTE2		0x280E
#define VMCS_GUEST_PDPTE2_HIGH		0x280F
#define VMCS_GUEST_PDPTE3		0x2810
#define VMCS_GUEST_PDPTE3_HIGH		0x2811
#define VMCS_GUEST_IA32_RTIT_CTL	0x2814

/* 64-Bit Host-State Fields */
#define VMCS_HOST_IA32_PAT		0x2C00
#define VMCS_HOST_IA32_PAT_HIGH		0x2C01
#define VMCS_HOST_IA32_EFER		0x2C02
#define VMCS_HOST_IA32_EFER_HIGH	0x2C03
#define VMCS_HOST_IA32_PERF_GLOBAL_CTRL	0x2C04
#define VMCS_HOST_IA32_PERF_GLOBAL_CTRL_HIGH	0x2C05

/* 32-Bit Control Fields */
#define VMCS_PIN_BASED_VMEXEC_CTL	0x4000
#define VMCS_PROC_BASED_VMEXEC_CTL	0x4002
#define VMCS_EXCEPTION_BMP		0x4004
#define VMCS_PAGEFAULT_ERRCODE_MASK	0x4006
#define VMCS_PAGEFAULT_ERRCODE_MATCH	0x4008
#define VMCS_CR3_TARGET_COUNT		0x400A
#define VMCS_VMEXIT_CTL			0x400C
#define VMCS_VMEXIT_MSR_STORE_COUNT	0x400E
#define VMCS_VMEXIT_MSR_LOAD_COUNT	0x4010
#define VMCS_VMENTRY_CTL		0x4012
#define VMCS_VMENTRY_MSR_LOAD_COUNT	0x4014
#define VMCS_VMENTRY_INTR_INFO_FIELD	0x4016
#define VMCS_VMENTRY_EXCEPTION_ERRCODE	0x4018
#define VMCS_VMENTRY_INSTRUCTION_LEN	0x401A
#define VMCS_TPR_THRESHOLD		0x401C
#define VMCS_PROC_BASED_VMEXEC_CTL2	0x401E

/* 32-Bit Read-Only Data Fields */
#define VMCS_VM_INSTRUCTION_ERR		0x4400
#define VMCS_EXIT_REASON		0x4402
#define VMCS_VMEXIT_INTR_INFO		0x4404
#define VMCS_VMEXIT_INTR_ERRCODE	0x4406
#define VMCS_IDT_VECTORING_INFO_FIELD	0x4408
#define VMCS_IDT_VECTORING_ERRCODE	0x440A
#define VMCS_VMEXIT_INSTRUCTION_LEN	0x440C
#define VMCS_VMEXIT_INSTRUCTION_INFO	0x440E
/* Note: VM-exit instruction information was formerly named as
 * VMX-instruction information */

/* 32-Bit Guest-State Fields */
#define VMCS_GUEST_ES_LIMIT		0x4800
#define VMCS_GUEST_CS_LIMIT		0x4802
#define VMCS_GUEST_SS_LIMIT		0x4804
#define VMCS_GUEST_DS_LIMIT		0x4806
#define VMCS_GUEST_FS_LIMIT		0x4808
#define VMCS_GUEST_GS_LIMIT		0x480A
#define VMCS_GUEST_LDTR_LIMIT		0x480C
#define VMCS_GUEST_TR_LIMIT		0x480E
#define VMCS_GUEST_GDTR_LIMIT		0x4810
#define VMCS_GUEST_IDTR_LIMIT		0x4812
#define VMCS_GUEST_ES_ACCESS_RIGHTS	0x4814
#define VMCS_GUEST_CS_ACCESS_RIGHTS	0x4816
#define VMCS_GUEST_SS_ACCESS_RIGHTS	0x4818
#define VMCS_GUEST_DS_ACCESS_RIGHTS	0x481A
#define VMCS_GUEST_FS_ACCESS_RIGHTS	0x481C
#define VMCS_GUEST_GS_ACCESS_RIGHTS	0x481E
#define VMCS_GUEST_LDTR_ACCESS_RIGHTS	0x4820
#define VMCS_GUEST_TR_ACCESS_RIGHTS	0x4822
#define VMCS_GUEST_INTERRUPTIBILITY_STATE	0x4824
#define VMCS_GUEST_ACTIVITY_STATE	0x4826
#define VMCS_GUEST_SMBASE		0x4828
#define VMCS_GUEST_IA32_SYSENTER_CS	0x482A

/* 32-Bit Host-State Field */
#define VMCS_HOST_IA32_SYSENTER_CS	0x4C00

/* Natural-Width Control Fields */
#define VMCS_CR0_GUESTHOST_MASK		0x6000
#define VMCS_CR4_GUESTHOST_MASK		0x6002
#define VMCS_CR0_READ_SHADOW		0x6004
#define VMCS_CR4_READ_SHADOW		0x6006
#define VMCS_CR3_TARGET_VALUE_0		0x6008
#define VMCS_CR3_TARGET_VALUE_1		0x600A
#define VMCS_CR3_TARGET_VALUE_2		0x600C
#define VMCS_CR3_TARGET_VALUE_3		0x600E

/* Natural-Width Read-Only Data Fields */
#define VMCS_EXIT_QUALIFICATION		0x6400
#define VMCS_IO_RCX			0x6402
#define VMCS_IO_RSI			0x6404
#define VMCS_IO_RDI			0x6406
#define VMCS_IO_RIP			0x6408
#define VMCS_GUEST_LINEAR_ADDR		0x640A

/* Natural-Width Guest-State Fields */
#define VMCS_GUEST_CR0			0x6800
#define VMCS_GUEST_CR3			0x6802
#define VMCS_GUEST_CR4			0x6804
#define VMCS_GUEST_ES_BASE		0x6806
#define VMCS_GUEST_CS_BASE		0x6808
#define VMCS_GUEST_SS_BASE		0x680A
#define VMCS_GUEST_DS_BASE		0x680C
#define VMCS_GUEST_FS_BASE		0x680E
#define VMCS_GUEST_GS_BASE		0x6810
#define VMCS_GUEST_LDTR_BASE		0x6812
#define VMCS_GUEST_TR_BASE		0x6814
#define VMCS_GUEST_GDTR_BASE		0x6816
#define VMCS_GUEST_IDTR_BASE		0x6818
#define VMCS_GUEST_DR7			0x681A
#define VMCS_GUEST_RSP			0x681C
#define VMCS_GUEST_RIP			0x681E
#define VMCS_GUEST_RFLAGS		0x6820
#define VMCS_GUEST_PENDING_DEBUG_EXCEPTIONS	0x6822
#define VMCS_GUEST_IA32_SYSENTER_ESP	0x6824
#define VMCS_GUEST_IA32_SYSENTER_EIP	0x6826

/* Natural-Width Host-State Fields */
#define VMCS_HOST_CR0			0x6C00
#define VMCS_HOST_CR3			0x6C02
#define VMCS_HOST_CR4			0x6C04
#define VMCS_HOST_FS_BASE		0x6C06
#define VMCS_HOST_GS_BASE		0x6C08
#define VMCS_HOST_TR_BASE		0x6C0A
#define VMCS_HOST_GDTR_BASE		0x6C0C
#define VMCS_HOST_IDTR_BASE		0x6C0E
#define VMCS_HOST_IA32_SYSENTER_ESP	0x6C10
#define VMCS_HOST_IA32_SYSENTER_EIP	0x6C12
#define VMCS_HOST_RSP			0x6C14
#define VMCS_HOST_RIP			0x6C16

#define VMCS_GUEST_INTERRUPTIBILITY_STATE_BLOCKING_BY_STI_BIT		0x1
#define VMCS_GUEST_INTERRUPTIBILITY_STATE_BLOCKING_BY_MOV_SS_BIT	0x2
#define VMCS_GUEST_INTERRUPTIBILITY_STATE_BLOCKING_BY_SMI_BIT		0x4
#define VMCS_GUEST_INTERRUPTIBILITY_STATE_BLOCKING_BY_NMI_BIT		0x8
#define VMCS_PIN_BASED_VMEXEC_CTL_EXINTEXIT_BIT	0x1
#define VMCS_PIN_BASED_VMEXEC_CTL_NMIEXIT_BIT	0x8
#define VMCS_PIN_BASED_VMEXEC_CTL_VIRTNMIS_BIT	0x20
#define VMCS_PROC_BASED_VMEXEC_CTL_INTRWINEXIT_BIT	0x4
#define VMCS_PROC_BASED_VMEXEC_CTL_USETSCOFF_BIT	0x8
#define VMCS_PROC_BASED_VMEXEC_CTL_HLTEXIT_BIT		0x80
#define VMCS_PROC_BASED_VMEXEC_CTL_INVLPGEXIT_BIT	0x200
#define VMCS_PROC_BASED_VMEXEC_CTL_MWAITEXIT_BIT	0x400
#define VMCS_PROC_BASED_VMEXEC_CTL_RDPMCEXIT_BIT	0x800
#define VMCS_PROC_BASED_VMEXEC_CTL_RDTSCEXIT_BIT	0x1000
#define VMCS_PROC_BASED_VMEXEC_CTL_CR3LOADEXIT_BIT	0x8000
#define VMCS_PROC_BASED_VMEXEC_CTL_CR3STOREEXIT_BIT	0x10000
#define VMCS_PROC_BASED_VMEXEC_CTL_CR8LOADEXIT_BIT	0x80000
#define VMCS_PROC_BASED_VMEXEC_CTL_CR8STOREEXIT_BIT	0x100000
#define VMCS_PROC_BASED_VMEXEC_CTL_USETPRSHADOW_BIT	0x200000
#define VMCS_PROC_BASED_VMEXEC_CTL_NMIWINEXIT_BIT	0x400000
#define VMCS_PROC_BASED_VMEXEC_CTL_MOVDREXIT_BIT	0x800000
#define VMCS_PROC_BASED_VMEXEC_CTL_UNCONDIOEXIT_BIT	0x1000000
#define VMCS_PROC_BASED_VMEXEC_CTL_USEIOBMP_BIT		0x2000000
#define VMCS_PROC_BASED_VMEXEC_CTL_USEMSRBMP_BIT	0x10000000
#define VMCS_PROC_BASED_VMEXEC_CTL_MONITOREXIT_BIT	0x20000000
#define VMCS_PROC_BASED_VMEXEC_CTL_PAUSEEXIT_BIT	0x40000000
#define VMCS_PROC_BASED_VMEXEC_CTL_ACTIVATECTLS2_BIT	0x80000000
#define VMCS_VMEXIT_CTL_HOST_ADDRESS_SPACE_SIZE_BIT 0x200
#define VMCS_VMEXIT_CTL_LOAD_IA32_PERF_GLOBAL_CTRL_BIT	0x1000
#define VMCS_VMEXIT_CTL_ACK_INTR_ON_EXIT_BIT	0x8000
#define VMCS_VMEXIT_CTL_SAVE_IA32_PAT_BIT	0x40000
#define VMCS_VMEXIT_CTL_LOAD_IA32_PAT_BIT	0x80000
#define VMCS_VMEXIT_CTL_SAVE_IA32_EFER_BIT	0x100000
#define VMCS_VMEXIT_CTL_LOAD_IA32_EFER_BIT	0x200000
#define VMCS_VMEXIT_CTL_CLEAR_IA32_RTIT_CTL_BIT	0x2000000
#define VMCS_VMENTRY_CTL_64_GUEST_BIT	0x200
#define VMCS_VMENTRY_CTL_LOAD_IA32_PAT_BIT	0x4000
#define VMCS_VMENTRY_CTL_LOAD_IA32_EFER_BIT	0x8000
#define VMCS_VMENTRY_CTL_LOAD_IA32_RTIT_CTL_BIT	0x40000
#define VMCS_PROC_BASED_VMEXEC_CTL2_ENABLE_EPT_BIT	0x2
#define VMCS_PROC_BASED_VMEXEC_CTL2_ENABLE_RDTSCP_BIT	0x8
#define VMCS_PROC_BASED_VMEXEC_CTL2_ENABLE_VPID_BIT	0x20
#define VMCS_PROC_BASED_VMEXEC_CTL2_UNRESTRICTED_GUEST_BIT	0x80
#define VMCS_PROC_BASED_VMEXEC_CTL2_ENABLE_INVPCID_BIT	0x1000
#define VMCS_PROC_BASED_VMEXEC_CTL2_ENABLE_VM_FUNCTIONS_BIT	0x2000
#define VMCS_PROC_BASED_VMEXEC_CTL2_ENABLE_VMCS_SHADOWING_BIT	0x4000
#define VMCS_PROC_BASED_VMEXEC_CTL2_ENABLE_XSAVES_BIT	0x100000
#define VMCS_PROC_BASED_VMEXEC_CTL2_PT_USES_GPHYS_BIT	0x1000000
#define VMCS_GUEST_ACTIVITY_STATE_ACTIVE	0x0
#define VMCS_GUEST_ACTIVITY_STATE_HLT		0x1
#define VMCS_GUEST_ACTIVITY_STATE_SHUTDOWN	0x2
#define VMCS_GUEST_ACTIVITY_STATE_WAIT_FOR_SIPI	0x3
#define VMCS_EPT_POINTER_EPT_WB		0x6
#define VMCS_EPT_PAGEWALK_LENGTH_4	0x18

#define VMXON_REGION_SIZE		0x1000
#define VMCS_REGION_SIZE		0x1000
#define ACCESS_RIGHTS_MASK		0xF0FF
#define ACCESS_RIGHTS_UNUSABLE_BIT	0x10000
#define ACCESS_RIGHTS_S_BIT		0x10
#define ACCESS_RIGHTS_P_BIT		0x80
#define ACCESS_RIGHTS_L_BIT		0x2000
#define ACCESS_RIGHTS_D_B_BIT		0x4000
#define ACCESS_RIGHTS_G_BIT		0x8000
#define SEL_PRIV_MASK			0x3
#define SEL_LDT_BIT			0x4
#define SEL_MASK			0xFFF8

#define EXIT_REASON_MASK		0xFFFF
#define EXIT_REASON_EXCEPTION_OR_NMI	0x0
#define EXIT_REASON_EXTERNAL_INT        0x1
#define EXIT_REASON_TRIPLE_FAULT	0x2
#define EXIT_REASON_INIT_SIGNAL		0x3
#define EXIT_REASON_STARTUP_IPI		0x4
#define EXIT_REASON_IO_SMI		0x5
#define EXIT_REASON_OTHER_SMI		0x6
#define EXIT_REASON_INTERRUPT_WINDOW	0x7
#define EXIT_REASON_NMI_WINDOW		0x8
#define EXIT_REASON_TASK_SWITCH		0x9
#define EXIT_REASON_CPUID		0xA
#define EXIT_REASON_GETSEC		0xB
#define EXIT_REASON_HLT			0xC
#define EXIT_REASON_INVD		0xD
#define EXIT_REASON_INVLPG		0xE
#define EXIT_REASON_RDPMC		0xF
#define EXIT_REASON_RDTSC		0x10
#define EXIT_REASON_RSM			0x11
#define EXIT_REASON_VMCALL		0x12
#define EXIT_REASON_VMCLEAR		0x13
#define EXIT_REASON_VMLAUNCH		0x14
#define EXIT_REASON_VMPTRLD		0x15
#define EXIT_REASON_VMPTRST		0x16
#define EXIT_REASON_VMREAD		0x17
#define EXIT_REASON_VMRESUME		0x18
#define EXIT_REASON_VMWRITE		0x19
#define EXIT_REASON_VMXOFF		0x1A
#define EXIT_REASON_VMXON		0x1B
#define EXIT_REASON_MOV_CR		0x1C
#define EXIT_REASON_MOV_DR		0x1D
#define EXIT_REASON_IO_INSTRUCTION	0x1E
#define EXIT_REASON_RDMSR		0x1F
#define EXIT_REASON_WRMSR		0x20
#define EXIT_REASON_ENTFAIL_GUEST_STATE	0x21
#define EXIT_REASON_ENTFAIL_MSR_LOADING	0x22
/* 0x23 is not used */
#define EXIT_REASON_MWAIT		0x24
#define EXIT_REASON_MONITOR_TRAP_FLAG	0x25
/* 0x26 is not used */
#define EXIT_REASON_MONITOR		0x27
#define EXIT_REASON_PAUSE		0x28
#define EXIT_REASON_ENTFAIL_MACHINE_CHK	0x29
/* 0x2A is not used */
#define EXIT_REASON_TPR_BELOW_THRESHOLD	0x2B
#define EXIT_REASON_VMEXIT_FROM_VMX_ROOT_OPERATION_BIT	0x20000000
#define EXIT_REASON_VMENTRY_FAILURE_BIT	0x80000000
#define EXIT_REASON_APIC_ACCESS		0x2C
#define EXIT_REASON_VIRTUALIZED_EOI	0x2D
#define EXIT_REASON_ACCESS_GDTR_OR_IDTR	0x2E
#define EXIT_REASON_ACCESS_LDTR_OR_TR	0x2F
#define EXIT_REASON_EPT_VIOLATION	0x30
#define EXIT_REASON_EPT_MISCONFIG	0x31
#define EXIT_REASON_INVEPT		0x32
#define EXIT_REASON_RDTSCP		0x33
#define EXIT_REASON_VMX_PREEMPT_TIMER	0x34
#define EXIT_REASON_INVVPID		0x35
#define EXIT_REASON_WBINVD		0x36
#define EXIT_REASON_XSETBV		0x37
#define EXIT_REASON_APIC_WRITE		0x38
#define EXIT_REASON_RDRAND		0x39
#define EXIT_REASON_INVPCID		0x3A
#define EXIT_REASON_VMFUNC		0x3B

#define VMEXIT_CR0_READ			0x0
#define VMEXIT_CR1_READ			0x1
#define VMEXIT_CR2_READ			0x2
#define VMEXIT_CR3_READ			0x3
#define VMEXIT_CR4_READ			0x4
#define VMEXIT_CR5_READ			0x5
#define VMEXIT_CR6_READ			0x6
#define VMEXIT_CR7_READ			0x7
#define VMEXIT_CR8_READ			0x8
#define VMEXIT_CR9_READ			0x9
#define VMEXIT_CR10_READ		0xA
#define VMEXIT_CR11_READ		0xB
#define VMEXIT_CR12_READ		0xC
#define VMEXIT_CR13_READ		0xD
#define VMEXIT_CR14_READ		0xE
#define VMEXIT_CR15_READ		0xF
#define VMEXIT_CR0_WRITE		0x10
#define VMEXIT_CR1_WRITE		0x11
#define VMEXIT_CR2_WRITE		0x12
#define VMEXIT_CR3_WRITE		0x13
#define VMEXIT_CR4_WRITE		0x14
#define VMEXIT_CR5_WRITE		0x15
#define VMEXIT_CR6_WRITE		0x16
#define VMEXIT_CR7_WRITE		0x17
#define VMEXIT_CR8_WRITE		0x18
#define VMEXIT_CR9_WRITE		0x19
#define VMEXIT_CR10_WRITE		0x1A
#define VMEXIT_CR11_WRITE		0x1B
#define VMEXIT_CR12_WRITE		0x1C
#define VMEXIT_CR13_WRITE		0x1D
#define VMEXIT_CR14_WRITE		0x1E
#define VMEXIT_CR15_WRITE		0x1F
#define VMEXIT_DR0_READ			0x20
#define VMEXIT_DR1_READ			0x21
#define VMEXIT_DR2_READ			0x22
#define VMEXIT_DR3_READ			0x23
#define VMEXIT_DR4_READ			0x24
#define VMEXIT_DR5_READ			0x25
#define VMEXIT_DR6_READ			0x26
#define VMEXIT_DR7_READ			0x27
#define VMEXIT_DR8_READ			0x28
#define VMEXIT_DR9_READ			0x29
#define VMEXIT_DR10_READ		0x2A
#define VMEXIT_DR11_READ		0x2B
#define VMEXIT_DR12_READ		0x2C
#define VMEXIT_DR13_READ		0x2D
#define VMEXIT_DR14_READ		0x2E
#define VMEXIT_DR15_READ		0x2F
#define VMEXIT_DR0_WRITE		0x30
#define VMEXIT_DR1_WRITE		0x31
#define VMEXIT_DR2_WRITE		0x32
#define VMEXIT_DR3_WRITE		0x33
#define VMEXIT_DR4_WRITE		0x34
#define VMEXIT_DR5_WRITE		0x35
#define VMEXIT_DR6_WRITE		0x36
#define VMEXIT_DR7_WRITE		0x37
#define VMEXIT_DR8_WRITE		0x38
#define VMEXIT_DR9_WRITE		0x39
#define VMEXIT_DR10_WRITE		0x3A
#define VMEXIT_DR11_WRITE		0x3B
#define VMEXIT_DR12_WRITE		0x3C
#define VMEXIT_DR13_WRITE		0x3D
#define VMEXIT_DR14_WRITE		0x3E
#define VMEXIT_DR15_WRITE		0x3F
#define VMEXIT_EXCP0			0x40
#define VMEXIT_EXCP1			0x41
#define VMEXIT_EXCP2			0x42
#define VMEXIT_EXCP3			0x43
#define VMEXIT_EXCP4			0x44
#define VMEXIT_EXCP5			0x45
#define VMEXIT_EXCP6			0x46
#define VMEXIT_EXCP7			0x47
#define VMEXIT_EXCP8			0x48
#define VMEXIT_EXCP9			0x49
#define VMEXIT_EXCP10			0x4A
#define VMEXIT_EXCP11			0x4B
#define VMEXIT_EXCP12			0x4C
#define VMEXIT_EXCP13			0x4D
#define VMEXIT_EXCP14			0x4E
#define VMEXIT_EXCP15			0x4F
#define VMEXIT_EXCP16			0x50
#define VMEXIT_EXCP17			0x51
#define VMEXIT_EXCP18			0x52
#define VMEXIT_EXCP19			0x53
#define VMEXIT_EXCP20			0x54
#define VMEXIT_EXCP21			0x55
#define VMEXIT_EXCP22			0x56
#define VMEXIT_EXCP23			0x57
#define VMEXIT_EXCP24			0x58
#define VMEXIT_EXCP25			0x59
#define VMEXIT_EXCP26			0x5A
#define VMEXIT_EXCP27			0x5B
#define VMEXIT_EXCP28			0x5C
#define VMEXIT_EXCP29			0x5D
#define VMEXIT_EXCP30			0x5E
#define VMEXIT_EXCP31			0x5F
#define VMEXIT_INTR			0x60
#define VMEXIT_NMI			0x61
#define VMEXIT_SMI			0x62
#define VMEXIT_INIT			0x63
#define VMEXIT_VINTR			0x64
#define VMEXIT_CR0_SEL_WRITE		0x65
#define VMEXIT_IDTR_READ		0x66
#define VMEXIT_GDTR_READ		0x67
#define VMEXIT_LDTR_READ		0x68
#define VMEXIT_TR_READ			0x69
#define VMEXIT_IDTR_WRITE		0x6A
#define VMEXIT_GDTR_WRITE		0x6B
#define VMEXIT_LDTR_WRITE		0x6C
#define VMEXIT_TR_WRITE			0x6D
#define VMEXIT_RDTSC			0x6E
#define VMEXIT_RDPMC			0x6F
#define VMEXIT_PUSHF			0x70
#define VMEXIT_POPF			0x71
#define VMEXIT_CPUID			0x72
#define VMEXIT_RSM			0x73
#define VMEXIT_IRET			0x74
#define VMEXIT_SWINT			0x75
#define VMEXIT_INVD			0x76
#define VMEXIT_PAUSE			0x77
#define VMEXIT_HLT			0x78
#define VMEXIT_INVLPG			0x79
#define VMEXIT_INVLPGA			0x7A
#define VMEXIT_IOIO			0x7B
#define VMEXIT_MSR			0x7C
#define VMEXIT_TASK_SWITCH		0x7D
#define VMEXIT_FERR_FREEZE		0x7E
#define VMEXIT_SHUTDOWN			0x7F
#define VMEXIT_VMRUN			0x80
#define VMEXIT_VMMCALL			0x81
#define VMEXIT_VMLOAD			0x82
#define VMEXIT_VMSAVE			0x83
#define VMEXIT_STGI			0x84
#define VMEXIT_CLGI			0x85
#define VMEXIT_SKINIT			0x86
#define VMEXIT_RDTSCP			0x87
#define VMEXIT_ICEBP			0x88
#define VMEXIT_WBINVD			0x89
#define VMEXIT_MONITOR			0x8A
#define VMEXIT_MWAIT			0x8B
#define VMEXIT_MWAIT_CONDITIONAL	0x8C
#define VMEXIT_NPF			0x400
#define VMEXIT_INVALID			-1

#define RFLAGS_CF_BIT			0x1
#define RFLAGS_PF_BIT			0x4
#define RFLAGS_AF_BIT			0x10
#define RFLAGS_ZF_BIT			0x40
#define RFLAGS_SF_BIT			0x80
#define RFLAGS_TF_BIT			0x100
#define RFLAGS_IF_BIT			0x200
#define RFLAGS_DF_BIT			0x400
#define RFLAGS_OF_BIT			0x800
#define RFLAGS_IOPL_MASK		0x3000
#define RFLAGS_IOPL_0			0x0000
#define RFLAGS_IOPL_1			0x1000
#define RFLAGS_IOPL_2			0x2000
#define RFLAGS_IOPL_3			0x3000
#define RFLAGS_NT_BIT			0x4000
#define RFLAGS_RF_BIT			0x10000
#define RFLAGS_VM_BIT			0x20000
#define RFLAGS_AC_BIT			0x40000
#define RFLAGS_VIF_BIT			0x80000
#define RFLAGS_VIP_BIT			0x100000
#define RFLAGS_ID_BIT			0x200000
#define RFLAGS_ALWAYS1_BIT		0x2
#define RFLAGS_SYS_MASK			RFLAGS_TF_BIT | \
					RFLAGS_IF_BIT | \
					RFLAGS_IOPL_MASK | \
					RFLAGS_NT_BIT | \
					RFLAGS_RF_BIT | \
					RFLAGS_VM_BIT | \
					RFLAGS_AC_BIT | \
					RFLAGS_VIF_BIT | \
					RFLAGS_VIP_BIT | \
					RFLAGS_ID_BIT
#define RFLAGS_NONSYS_MASK		RFLAGS_CF_BIT | \
					RFLAGS_PF_BIT | \
					RFLAGS_AF_BIT | \
					RFLAGS_ZF_BIT | \
					RFLAGS_SF_BIT | \
					RFLAGS_DF_BIT | \
					RFLAGS_OF_BIT
#define CR0_PE_BIT			0x1
#define CR0_MP_BIT			0x2
#define CR0_EM_BIT			0x4
#define CR0_TS_BIT			0x8
#define CR0_ET_BIT			0x10
#define CR0_NE_BIT			0x20
#define CR0_WP_BIT			0x10000
#define CR0_AM_BIT			0x40000
#define CR0_NW_BIT			0x20000000
#define CR0_CD_BIT			0x40000000
#define CR0_PG_BIT			0x80000000

#define CR3_PWT_BIT			0x8
#define CR3_PCD_BIT			0x10
#define CR3_PCID_KEEPTLB_BIT		0x8000000000000000ULL

#define CR4_VME_BIT			0x1
#define CR4_PVI_BIT			0x2
#define CR4_TSD_BIT			0x4
#define CR4_DE_BIT			0x8
#define CR4_PSE_BIT			0x10
#define CR4_PAE_BIT			0x20
#define CR4_MCE_BIT			0x40
#define CR4_PGE_BIT			0x80
#define CR4_PCE_BIT			0x100
#define CR4_OSFXSR_BIT			0x200
#define CR4_OSXMMEXCPT_BIT		0x400
#define CR4_VMXE_BIT			0x2000
#define CR4_PCIDE_BIT			0x20000
#define CR4_OSXSAVE_BIT			0x40000
#define CR4_PKE_BIT			0x400000

#define XCR0_X87_STATE_BIT		0x1
#define XCR0_SSE_STATE_BIT		0x2
#define XCR0_AVX_STATE_BIT		0x4
#define XCR0_BNDREGS_STATE_BIT		0x8
#define XCR0_BNDCSR_STATE_BIT		0x10
#define XCR0_OPMASK_STATE_BIT		0x20
#define XCR0_ZMM_HI256_STATE_BIT	0x40
#define XCR0_HI16_ZMM_STATE_BIT		0x80

#define PDE_P_BIT			0x1
#define PDE_RW_BIT			0x2
#define PDE_US_BIT			0x4
#define PDE_PWT_BIT			0x8
#define PDE_PCD_BIT			0x10
#define PDE_A_BIT			0x20
#define PDE_D_BIT			0x40
#define PDE_PS_BIT			0x80
#define PDE_G_BIT			0x100
#define PDE_AVAILABLE1_BIT		0x200
#define PDE_AVAILABLE2_BIT		0x400
#define PDE_AVAILABLE3_BIT		0x800
#define PDE_AVAILABLE_MASK		0xE00
#define PDE_NX_BIT			0x8000000000000000ULL
#define PDE_4K_ADDR_MASK		0xFFFFF000
#define PDE_ATTR_MASK			0xFFF
#define PDE_PS_PAT_BIT			0x1000
#define PDE_4M_PAT_BIT			PDE_PS_PAT_BIT
#define PDE_4M_OFFSET_MASK		0x003FFFFF
#define PDE_4M_ADDR_MASK		(~PDE_4M_OFFSET_MASK)
#define PDE_2M_PAT_BIT			PDE_PS_PAT_BIT
#define PDE_2M_OFFSET_MASK		0x001FFFFF
#define PDE_2M_ADDR_MASK		(~PDE_2M_OFFSET_MASK)
#define PDE_ADDR_MASK64			0x0000000FFFFFF000ULL

#define PTE_P_BIT			0x1
#define PTE_RW_BIT			0x2
#define PTE_US_BIT			0x4
#define PTE_PWT_BIT			0x8
#define PTE_PCD_BIT			0x10
#define PTE_A_BIT			0x20
#define PTE_D_BIT			0x40
#define PTE_PAT_BIT			0x80
#define PTE_G_BIT			0x100
#define PTE_AVAILABLE1_BIT		0x200
#define PTE_AVAILABLE2_BIT		0x400
#define PTE_AVAILABLE3_BIT		0x800
#define PTE_AVAILABLE_MASK		0xE00
#define PTE_NX_BIT			0x8000000000000000ULL
#define PTE_ADDR_MASK			0xFFFFF000
#define PTE_ATTR_MASK			0xFFF
#define PTE_ADDR_MASK64			0x0000000FFFFFF000ULL

#define EXCEPTION_DE			0x0
#define EXCEPTION_DB			0x1
#define EXCEPTION_NMI			0x2
#define EXCEPTION_BP			0x3
#define EXCEPTION_OF			0x4
#define EXCEPTION_BR			0x5
#define EXCEPTION_UD			0x6
#define EXCEPTION_NM			0x7
#define EXCEPTION_DF			0x8
#define EXCEPTION_COPROCESSOR_SEGMENT_OVERRUN	0x9
#define EXCEPTION_TS			0xA
#define EXCEPTION_NP			0xB
#define EXCEPTION_SS			0xC
#define EXCEPTION_GP			0xD
#define EXCEPTION_PF			0xE
#define EXCEPTION_MF			0x10
#define EXCEPTION_AC			0x11
#define EXCEPTION_MC			0x12
#define EXCEPTION_XM			0x13
#define EXCEPTION_SX			0x1E

#define NUM_OF_IOPORT			0x10000
#define NUM_OF_INT			0x100

#define PAGEFAULT_ERR_P_BIT		0x1
#define PAGEFAULT_ERR_WR_BIT		0x2
#define PAGEFAULT_ERR_US_BIT		0x4
#define PAGEFAULT_ERR_RSVD_BIT		0x8
#define PAGEFAULT_ERR_ID_BIT		0x10

#define EXCEPTION_ERR_EXT_BIT		0x1
#define EXCEPTION_ERR_IDT_BIT		0x2
#define EXCEPTION_ERR_TI_BIT		0x4
#define EXCEPTION_ERR_SEL_MASK		0xFFF8
#define EXCEPTION_ERR_SEL_SHIFT		3

#define PAGESIZE			0x1000
#define PAGESIZE2M			0x200000
#define PAGESIZE4M			0x400000
#define PAGESIZE1G			0x40000000
#define PAGESIZE_SHIFT			12
#define PAGESIZE2M_SHIFT		21
#define PAGESIZE4M_SHIFT		22
#define PAGESIZE1G_SHIFT		30
#define PAGESIZE_MASK			(PAGESIZE - 1)
#define PAGESIZE2M_MASK			(PAGESIZE2M - 1)
#define PAGESIZE4M_MASK			(PAGESIZE4M - 1)
#define PAGESIZE1G_MASK			(PAGESIZE1G - 1)

#define PIT_COUNTER0			0x40
#define PIT_COUNTER1			0x41
#define PIT_COUNTER2			0x42
#define PIT_CONTROL			0x43
#define PIT_CONTROL_BINARY		0x0
#define PIT_CONTROL_BCD			0x1
#define PIT_CONTROL_MODE0		0x0
#define PIT_CONTROL_MODE1		0x2
#define PIT_CONTROL_MODE2		0x4
#define PIT_CONTROL_MODE3		0x6
#define PIT_CONTROL_MODE4		0x8
#define PIT_CONTROL_MODE5		0xA
#define PIT_CONTROL_LATCH		0x00
#define PIT_CONTROL_LOW8BIT		0x10
#define PIT_CONTROL_HIGH8BIT		0x20
#define PIT_CONTROL_16BIT		0x30
#define PIT_CONTROL_COUNTER0		0x00
#define PIT_CONTROL_COUNTER1		0x40
#define PIT_CONTROL_COUNTER2		0x80
#define PIT_CONTROL_READBACK		0xC0

#define VMM_MINSTACKSIZE		3072

#endif
