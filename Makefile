AS=lasm
KERMITASM=cpscmd.asm cpscpm.asm cpsdef.asm cpsmit.asm cpspk2.asm	\
	  cpsser.asm cpsutl.asm cpscom.asm cpsdat.asm cpsker.asm	\
	  cpspk1.asm cpsrem.asm cpstt.asm cpswld.asm
SYSTEMASM=cpxlnk.asm cpxtyp.asm cpxlnk.asm cpxcom.asm cpxswt.asm

cpsker.hex: $(KERMITASM)
	$(AS) --listing cpsker.asm

cpvtpt.hex: $(SYSTEMASM)
	sed -r -e 's/(trs80pt[ \t]+equ[ \t]+)false(.*)/\1TRUE\2/i' cpxtyp.asm > cpvtpt.asm
	$(AS) cpvtpt.asm
	rm cpvtpt.asm

cpvjai.hex: $(SYSTEMASM) cpxhea.asm
	sed -r -e 's/(jair[ \t]+equ[ \t]+)false(.*)/\1TRUE\2/i' -e 's/(crt[ \t]+equ[ \t]+)false(.*)/\1TRUE\2/i' cpxtyp.asm > cpvjai.asm
	$(AS) --listing cpvjai.asm
	rm cpvjai.asm
