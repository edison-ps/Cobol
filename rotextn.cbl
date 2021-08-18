      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  ROTEXTN      *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Rotina para extenso :                                      *
      *                                                             *
      *  Data da ultima alteracao:    08/03/94     v1.00            *
      *                                                             *
      ***************************************************************
      *
       identification division.
       program-id. rotextn.
       author. Edisom Pires de Souza.
      *
       environment division.
       configuration section.
      *
       special-names.
           decimal-point is comma.
      *
       data division.
       working-storage section.
      *
       01 campos.
          02  coluna               pic 9(03) value 0.
          02  valor-aux            pic 9(12) value 0.
          02  resto                pic 9(12) value 0.
          02  resposta             pic x.
      *
       01 valor-aux1.
          02 aux1-int              pic 9(12) value 0.
          02 aux1-dec              pic 9(02) value 0.
      *
       01  extmon.
           02 filler pic x(32) value "Um Dois Tres Qua-tro Cin-co Seis".
           02 filler pic x(32) value " Se-te Oi-to No-ve Dez On-ze Do-".
           02 filler pic x(32) value "ze Tre-ze Qua-tor-ze Quin-ze De-".
           02 filler pic x(32) value "zes-seis De-zes-se-te De-zoi-to ".
           02 filler pic x(32) value "De-ze-no-ve Vin-te Trin-ta Qua-r".
           02 filler pic x(32) value "en-ta Cin-quen-ta Ses-sen-ta Se-". 
           02 filler pic x(32) value "ten-ta Oi-ten-ta No-ven-ta Cen-t".
           02 filler pic x(32) value "o Du-zen-tos Tre-zen-tos Qua-tro".
           02 filler pic x(32) value "-cen-tos Qui-nhen-tos Seis-cen-t".
           02 filler pic x(32) value "os Se-te-cen-tos Oi-to-cen-tos N".
           02 filler pic x(32) value "o-ve-cen-tos Mil Mi-lhao Mi-lhoe".
           02 filler pic x(32) value "s Cru-zei-ros Cen-ta-vos Bi-lhao".
           02 filler pic x(17) value " Bi-lhoes Re-ais ".
      *
       01  tb-extr   redefines extmon.
           02 ext    pic x(01) occurs 401.
      *
       01  tb-und.
           02 filler pic x(32) value "00000300801302102803303904505105".
           02 filler pic x(25) value "5061067074085093105118128".
      *
       01  tb-undr   redefines tb-und.
           02 und    pic 9(03) occurs 19.
      *
       01  tb-dzn.
           02 filler pic x(27) value "000140147155166178189199209".
      *
       01  tb-dznr   redefines tb-dzn.
           02 dzn    pic 9(03) occurs 9.
      *
       01  tb-cnt.
           02 filler pic x(27) value "219226237249265278291305319".
      *
       01  tb-cntr   redefines tb-cnt.
           02 cnt    pic 9(03) occurs 9.
      *
       01  vlr-aux.
           02 vlr-cnt    pic 9(01).
           02 vlr-dzu    pic 9(02).
           02 vlr-dzur   redefines vlr-dzu.
              03 vlr-dzn pic 9(01).
              03 vlr-und pic 9(01).
      *
       01  diversos.
           02 dig-mcv    pic 9(03).
           02 ctd-col    pic 9(03).
           02 idx-ext    pic 9(03).
           02 idx-lnh    pic 9(03).
           02 cmp-lnh-5  pic 9(03).
           02 i          pic 9(02).    
           02 j          pic 9(03).
           02 k          pic 9(03).
           02 l          pic 9(03).
           02 m          pic 9(03).
     *
       01  tb-lnh.
           02 lnh        pic x(01) occurs 300.
     *

       01  campo-extenso.
           02 vlr-int    pic 9(12).
           02 vlr-ctv    pic 9(02).
           02 vlr-blh    pic 9(03).
           02 vlr-mlh    pic 9(03).
           02 vlr-mil    pic 9(03).
           02 vlr-crz    pic 9(03).
           02 cmp-lnh    pic 9(03).
           02 lnh-300    value spaces.
              03 lnh-chr pic x(01) occurs 300.
      *
       linkage section.
      *
       01 campo-valor.
          02 val-valor                 pic 9(12)v9(02).
          02 val-tam                   pic 9(03).
          02 val-extenso               pic x(300).
      *
       procedure division using campo-valor.
      *
       inicio.
           move val-tam to cmp-lnh.
           move val-valor to vlr-int valor-aux.
           divide valor-aux by 1000000000 giving vlr-blh
                  remainder resto.
           move resto to valor-aux.
           divide valor-aux by 1000000 giving vlr-mlh
                  remainder resto.
           move resto to valor-aux.
           divide valor-aux by 1000 giving vlr-mil
                  remainder vlr-crz.
      *     move 0 to vlr-ctv.
           move val-valor to valor-aux1.
           move aux1-dec to vlr-ctv.
           move 0 to idx-ext idx-lnh ctd-col.
           subtract 5 from cmp-lnh giving cmp-lnh-5.
           move all " " to tb-lnh lnh-300.
           if vlr-blh = 0
              go to ni-mlh.
           if vlr-blh = 1
              move 377 to dig-mcv
           else 
              move 385 to dig-mcv.
           move vlr-blh to vlr-aux.
           perform sbr-ext thru ni-vlr.
           move 0 to m.
           if vlr-mlh not = 0
              add 1 to m.
           if vlr-mil not = 0
              add 1 to m.
           if vlr-crz not = 0
              add 1 to m.
           if m = 0
              add 1 to idx-lnh
              move "d" to lnh (idx-lnh)
              perform sbr-eee
              go to ni-crz.
           if m = 1
              perform sbr-eee
           else
              perform sbr-vrg.
      *
       ni-mlh.
           if vlr-mlh = 0 go to ni-mil.
           if vlr-mlh = 1
              move 337 to dig-mcv
           else
              move 345 to dig-mcv.
           move vlr-mlh to vlr-aux.
           perform sbr-ext thru ni-vlr.
           move 0 to m.
           if vlr-mil not = 0
              add 1 to m.
           if vlr-crz not = 0
              add 1 to m.
           if m = 0
              add 1 to idx-lnh
              move "d" to lnh (idx-lnh)
              perform sbr-eee
              go to ni-crz.
           if m = 1
              perform sbr-eee
           else
              perform sbr-vrg.
      *
       ni-mil.
           if vlr-mil = 0
              go to ni-crz.
           move 333 to dig-mcv.
           move vlr-mil to vlr-aux.
           perform sbr-ext thru ni-vlr.
      *
       ni-crz.
           if 0 = vlr-int
              go to ni-ctv.
           if vlr-crz = 1 and vlr-int < 2
              move space to ext (365) ext (400)
              move "l" to ext (399).
           move vlr-crz to vlr-aux.
      *     move 354 to dig-mcv.         <---- Tira Cruzerios
      *     perform sbr-ext thru ni-vlr.

           move 394 to dig-mcv.        
           perform sbr-ext thru ni-vlr.


      *     move 394 to dig-mcv.
      *     perform ni-vlr.

           move "s" to ext (365).
           move "i" to ext (399).
           move "s" to ext (400).
           if vlr-ctv not = 0
              perform sbr-eee. 
      *
       ni-ctv.
           if vlr-ctv = 0
              go to ni-lnh.
           if vlr-ctv = 1
              move space to ext (376).
           move vlr-ctv to vlr-dzu.
           move 366 to dig-mcv.
           perform ct thru ni-vlr.
           move "s" to ext (376).
      *
       ni-lnh.
           if lnh (1) not = " " perform sbr-imp.
           move 300 to coluna.
      *
       ni-lnh1.
           if lnh-chr (coluna) = space
              move "*" to lnh-chr (coluna)
              subtract 1 from coluna
              go to ni-lnh1
           else
              move lnh-300 to val-extenso
           end-if.
           exit program.
      *      
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *
       sbr-ext.
           if vlr-cnt = 1 and vlr-dzu = 0
              move "m" to ext (222)
              move space to ext (223)
           else
              move "n" to ext (222)
              move "-" to ext (223).
           if vlr-cnt > 0
              move cnt (vlr-cnt) to idx-ext
              perform sbr-evl thru saida-evl.
           if vlr-dzu = 0
              go to ni-vlr.
           if vlr-cnt not = 0
              perform sbr-eee.
      *
       ct.
           if vlr-dzu < 20
              move und (vlr-dzu) to idx-ext
              go to ni-evl.
           move dzn (vlr-dzn) to idx-ext.
           perform sbr-evl thru saida-evl. 
           if vlr-und = 0
              go to ni-vlr.
           perform sbr-eee.
           move und (vlr-und) to idx-ext.
      *
       ni-evl.
           perform sbr-evl thru saida-evl.
      *
       ni-vlr.
           move dig-mcv to idx-ext.
           perform sbr-evl thru saida-evl.
      *
       sbr-evl.
           add 1 to idx-lnh.
      *
       ni-add.
           add 1 to idx-ext.
           move ext (idx-ext) to lnh (idx-lnh).
           if ext (idx-ext) = space
              go to saida-evl.
           if ext (idx-ext) not = "-"
              go to sbr-evl.
           if idx-lnh < cmp-lnh-5
              go to ni-add.
           perform ni-blk.
           go to sbr-evl.
      *
       saida-evl.
           if idx-lnh > cmp-lnh-5
              perform ni-blk.
      *
       sbr-eee.
           add 1 to idx-lnh.
           move "e" to lnh (idx-lnh).
           add 1 to idx-lnh.
           move space to lnh (idx-lnh).
           if idx-lnh > cmp-lnh-5
              perform ni-blk.
      *
       sbr-vrg.
           if idx-lnh < cmp-lnh and > 0
              move "," to lnh (idx-lnh)        
              add 1 to idx-lnh
              move space to lnh (idx-lnh).
           if idx-lnh > cmp-lnh-5
              perform ni-blk.
      *
       ni-blk.
           add 1 to idx-lnh.
           perform sbr-blk until idx-lnh > cmp-lnh.
           perform sbr-imp.
      *
       sbr-blk.
           move space to lnh (idx-lnh).
           add 1 to idx-lnh.
      *
       sbr-imp.
           move 0 to idx-lnh.
           perform reorganiza thru sai-reorganiza.
           move all " " to tb-lnh.
      *
       reorganiza.
           move 0 to i j k.
           move cmp-lnh to l.
      *
       lx.
           if lnh (l) = space
              add 1 to i
              subtract 1 from l
              go to lx.
      *
       xis.
           add 1 to j.
           add 1 to k.
           add 1 to ctd-col.
           move lnh (j) to lnh-chr (ctd-col).
           if lnh (j) = space and i > 0
              add 1 to k
              add 1 to ctd-col
              move space to lnh-chr (ctd-col)
              subtract 1 from i.
           if k < cmp-lnh
              go to xis.
      *
       sai-reorganiza.
           exit.