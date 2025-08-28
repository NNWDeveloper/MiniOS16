MiniOS16 1.x
============

MiniOS16 je jednoduchý 16bitový operační systém napsaný v jazyku symbolických adres.
Bootuje přímo z diskového obrazu (.img) a poskytuje základní shell s několika
příkazy.

------------------------------------------------------------
Jak spustit MiniOS16
------------------------------------------------------------

1) Vytvoření bootovacího obrazu
   - V adresáři musí být soubory:
       boot.bin   (bootloader)
       kernel.bin (kernel s příkazy)
   - Spojení do jednoho obrazu:
       copy /b boot.bin+kernel.bin os.img

2) Spuštění v QEMU (doporučeno):
       qemu-system-i386 -fda os.img

   Případně v DOSBoxu nebo na reálném PC s disketovou mechanikou
   (pozor: přepíše obsah diskety).

------------------------------------------------------------
Dostupné příkazy v shellu
------------------------------------------------------------

- help     : vypíše seznam dostupných příkazů
- cls      : vyčistí obrazovku
- echo     : vypíše text zadaný uživatelem
- mem      : zobrazí velikost konvenční paměti
- beep     : krátký zvukový signál
- about    : informace o OS
- reboot   : restart systému
- halt     : zastavení CPU (čeká na reset)
- sum      : sečte dvě čísla
- hex      : zobrazí číslo v hexadecimálním formátu
- hexdump  : vypíše obsah paměti v hexadecimálním tvaru

------------------------------------------------------------
Poznámky
------------------------------------------------------------

- OS běží v reálném režimu (16bit, BIOS interrupt služby).
- Nepodporuje multitasking ani souborový systém.
- Vhodný pro učení základů OS developmentu v assembleru.
- Testováno v QEMU, možné spustit i na reálném PC z diskety.

------------------------------------------------------------
Autor
------------------------------------------------------------
NNW Developer
https://nnwdev.fun
MiniOS16 byl vytvořen jako učební projekt. 
Verze: 1.2
