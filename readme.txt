+-----------------------------------------------------------------------------
| �^�C�g�� | ANS.1 (BER encording) to text converter
|�t�@�C����| ANS2TXT.ZIP
| �J�e�S�� | MS-DOS Application �i* 16bit application�j
| ����@�� | NEC��V30 series, intel��i80186�ȍ~(more than it).
|�O��\�t�g| MS-DOS version 3.30d�ȍ~(more than it)(64bit version��OS�͕s��)
| ���k���� | lh5
|�]�ڂ̉�| ���f�]�ڕs�iReproduction forbidden without courtesy.�j
|  ��  �l  | 
+-----------------------------------------------------------------------------

��[�a��(Japanese)]
-------------------------
���̃A�v���P�[�V�����́AASN.1��BER�G���R�[�f�B���O���ꂽ�f�[�^���A
�e�L�X�g�ɐ��`����v���O�����ł��B

X.509�i�d�q�ؖ����j��APKCS#7�i�d�q�����A�Í����b�Z�[�W�j���A
ASN.1��BER�G���R�[�f�B���O���ꂽ�f�[�^�𒭂߂�ׂɍ�����\�t�g�ł��B

 �� 64kByte�܂ł̃t�@�C���ɂ����Ή����Ă��܂���B
�@�@16bit�A�v���P�[�V�����Ȃ̂ŁADOS�� or �R�}���h�v�����v�g�Ŏg���Ă��������B



��[English(�p��)]
-------------------------
This application convert from BER encoded ASN.1 to text file.

* This is MS-DOS (16bit) application.
  Because, this application can convert under 64 kbyte file.



��[Output format]
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



����̃t�@�C���́yANS2TXT.ZIP�z�ł��B
