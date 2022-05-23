# EPT

Basically, memos are hardcoded in source code.

## EPT Tables specifications

PML4 (`N` is the physical-addr width supported by the processor. in x86-64 of 4-level pagin, `N = 48`):
![https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html](./img/pml4spec.png)

PDPT (Note that PDPT entry can directly maps 1GB page and the meaning of PDPTE differs in that case, though BitVisor doesn't support 1GB page):
![https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html](./img/pdptspec.png)

PD (Note that PDE can directly maps 2MB page depending on 7th bit of PDE, which BitVisor supports):

![https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html](./img/pdspec-2mb.png)
![https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html](./img/pdspec.png)

PT

![https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html](./img/ptspec.png)

**SUMMARY**

![https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html](./img/ptsummary.png)


### Notes

- EPT structure is present iff. **any** bits of PTE[2:0] is 1.
