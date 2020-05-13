+-----------------------------------------------------------------------------
| タイトル | ANS.1 (BER encording) to text converter
|ファイル名| ANS2TXT.ZIP
| カテゴリ | MS-DOS Application （* 16bit application）
| 動作機種 | NEC社V30 series, intel社i80186以降(more than it).
|前提ソフト| MS-DOS version 3.30d以降(more than it)(64bit versionのOSは不可)
| 圧縮方式 | lh5
|転載の可否| 無断転載不可（Reproduction forbidden without courtesy.）
|  備  考  | 
+-----------------------------------------------------------------------------

◆[和文(Japanese)]
-------------------------
このアプリケーションは、ASN.1のBERエンコーディングされたデータを、
テキストに整形するプログラムです。

X.509（電子証明書）や、PKCS#7（電子署名、暗号メッセージ）等、
ASN.1のBERエンコーディングされたデータを眺める為に作ったソフトです。

 ※ 64kByteまでのファイルにしか対応していません。
　　16bitアプリケーションなので、DOS窓 or コマンドプロンプトで使ってください。



◆[English(英文)]
-------------------------
This application convert from BER encoded ASN.1 to text file.

* This is MS-DOS (16bit) application.
  Because, this application can convert under 64 kbyte file.



◆[Output format]
-------------------------
XXXXh:nn nn nn nn              :text
XXXXh:nn nn nn nn nn nn nn nn
      nn nn nn nn nn           :text
....

	XXXXh : Address
	nn    : Hex code of original binary data.
	text  : ASN.1's TAG and value (::= value)

ex.)
0000h:30 82 02 6F              :Sequence{
0004h:06 09 2A 86 48 86 F7 0D 
      01 07 02                 : Object Identifier ::= 1.2.840.113549.1.7.2
000Fh:A0 82 02 60              : Context[0]{
0013h:30 82 02 5C              :  Sequence{
0017h:02 01 01                 :   Integer ::= 01 
001Ah:31 00                    :   Set{
001Ch:                         :   }
001Ch:30 0B                    :   Sequence{
001Eh:06 09 2A 86 48 86 F7 0D 
      01 07 01                 :    Object Identifier ::= 1.2.840.113549.1.7.1
0029h:                         :   }
0029h:A0 82 02 44              :   Context[0]{
....



製作のファイルは【ANS2TXT.ZIP】です。
