      ***************************************************************
      *                                                             *
      *  A B A V / S P -  I N F O R M A T I C A   :::  INIC001      *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Inicializador de arqquivos :                               *
      *                                                             *
      *  Data da ultima alteracao:    05/01/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. inic001.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
            select arqusr assign to disk
                   organization is indexed
                   access mode is random
                   lock mode is manual
                   with lock on record
                   record key is usr-chave
                   file status is usr-status.
      *
            select arqimp assign to disk
                   organization is indexed
                   access mode is dynamic
                   lock mode is manual
                   with lock on record
                   record key is imp-chave
                   file status is imp-status.
      *      
           select arqbx01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is bx01-chave
                  alternate record key is bx01-chave-1 with duplicates
                  alternate record key is bx01-chave-2 with duplicates
                  file status is rc01-status.
      *
           select arqrc01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is rc01-chave
                  alternate record key is rc01-chave-1 with duplicates
                  alternate record key is rc01-chave-2 with duplicates
                  file status is rc01-status.
      *
           select arqab01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab01-chave
                  alternate record key is ab01-chave-1 with duplicates
                  alternate record key is ab01-chave-2 with duplicates
                  file status is ab01-status.
      *
           select arqtabl assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on record
                  record key is tabl-chave
                  alternate record key is tabl-chave-1 with duplicates
                  file status is tabl-status. 
      *
           select arqab02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab02-chave
                  alternate record key is ab02-chave-1 with duplicates
                  file status is ab02-status.
      *
           select arqab03 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab03-chave
                  alternate record key is ab03-chave-1
                  file status is ab02-status.
      *
           select arqab04 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab04-chave 
                  alternate record key is ab04-chave-1 with duplicates
                  alternate record key is ab04-chave-2 with duplicates
                  file status is ab04-status.
      *
           select arqsl01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is sl01-chave
                  alternate record key is sl01-chave-1 with duplicates
                  alternate record key is sl01-chave-2 with duplicates
                  alternate record key is sl01-chave-3 with duplicates
                  alternate record key is sl01-chave-4 with duplicates
                  file status is sl01-status.
      *
           select arqcd01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is cd01-chave
                  alternate record key is cd01-chave-1 with duplicates
                  alternate record key is cd01-chave-2 with duplicates
                  alternate record key is cd01-chave-3 with duplicates
                  alternate record key is cd01-chave-4 with duplicates
                  file status is cd01-status.
      *
           select arqcd02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is cd02-chave
                  alternate record key is cd02-chave-1 with duplicates
                  file status is cd02-status.
      *
       data division.
       file section.
      *           
       copy fdusr.lib.
      *    
       copy fdimp.lib.
      *
       copy fdbx01.lib.
      *    
       copy fdrc01.lib.
      *    
       copy fdab01.lib.
      *
       copy fdtabl.lib.
      *    
       copy fdab02.lib.
      *    
       copy fdab03.lib.
      *    
       copy fdab04.lib.
      *
       copy fdsl01.lib.
      *
       copy fdcd01.lib.
      *
       copy fdcd02.lib.
      *
       working-storage section.
      
       01 usr-status                   pic x(02) value "00".
       01 usr-stat                     pic x(01) value "F".
      *
       01 nome-arq-usr.
          02 usr-dir                   pic x(03) value "USR".
          02 filler                    pic x(01) value "\".
          02 usr-nome                  pic x(08) value "ARQUSR00".
          02 filler                    pic x(01) value ".".
          02 usr-ext                   pic x(03) value "DAT".
      *
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 nome-arq-imp.
          02 imp-dir                   pic x(03) value "IMP".
          02 filler                    pic x(01) value "\".
          02 imp-nome                  pic x(07) value "ARQIMPA".
          02 filler                    pic x(01) value ".".
          02 imp-ext                   pic x(03) value "DAT".
      *
       01 bx01-status                  pic x(02) value "00".
       01 bx01-stat                    pic x(01) value "F".
      *
       01 nome-arq-bx01.
          02 bx01-dir                  pic x(03) value "RC2".
          02 filler                    pic x(01) value "\".
          02 bx01-nome                 pic x(08) value "ARQBX01A".
          02 filler                    pic x(01) value ".".
          02 bx01-ext                  pic x(03) value "DAT".
      *
       01 rc01-status                  pic x(02) value "00".
       01 rc01-stat                    pic x(01) value "F".
      *
       01 nome-arq-rc01.
          02 rc01-dir                  pic x(03) value "RC2".
          02 filler                    pic x(01) value "\".
          02 rc01-nome                 pic x(08) value "ARQRC01A".
          02 filler                    pic x(01) value ".".
          02 rc01-ext                  pic x(03) value "DAT".
      *
       01 ab01-status                  pic x(02) value "00".
       01 ab01-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab01.
          02 ab01-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab01-nome                 pic x(08) value "ARQAB01A".
          02 filler                    pic x(01) value ".".
          02 ab01-ext                  pic x(03) value "DAT".
      *
       01 tabl-status                  pic x(02) value "00".
       01 tabl-stat                    pic x(01) value "F".
      *
       01 nome-arq-tabl.
          02 tabl-dir                  pic x(03) value "TBL".
          02 filler                    pic x(01) value "\".
          02 tabl-nome                 pic x(08) value "ARQTABLA".
          02 filler                    pic x(01) value ".".
          02 tabl-ext                  pic x(03) value "DAT".
      *
       01 ab02-status                  pic x(02) value "00".
       01 ab02-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab02.
          02 ab02-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab02-nome                 pic x(08) value "ARQAB02A".
          02 filler                    pic x(01) value ".".
          02 ab02-ext                  pic x(03) value "DAT".
      *
       01 ab03-status                  pic x(02) value "00".
       01 ab03-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab03.
          02 ab03-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab03-nome                 pic x(08) value "ARQAB03A".
          02 filler                    pic x(01) value ".".
          02 ab03-ext                  pic x(03) value "DAT".
      *
       01 ab04-status                  pic x(02) value "00".
       01 ab04-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab04.
          02 ab04-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab04-nome                 pic x(08) value "ARQAB04A".
          02 filler                    pic x(01) value ".".
          02 ab04-ext                  pic x(03) value "DAT".
      *
       01 sl01-status                  pic x(02) value "00".
       01 sl01-stat                    pic x(01) value "F".
      *
       01 nome-arq-sl01.
          02 sl01-dir                  pic x(03) value "SL1".
          02 filler                    pic x(01) value "\".
          02 sl01-nome                 pic x(08) value "ARQSL01A".
          02 filler                    pic x(01) value ".".
          02 sl01-ext                  pic x(03) value "DAT".
      *
       01 cd01-status                  pic x(02) value "00".
       01 cd01-stat                    pic x(01) value "F".
      *
       01 nome-arq-cd01.
          02 cd01-dir                  pic x(03) value "CD1".
          02 filler                    pic x(01) value "\".
          02 cd01-nome                 pic x(08) value "ARQCD01A".
          02 filler                    pic x(01) value ".".
          02 cd01-ext                  pic x(03) value "DAT".
      *
       01 cd02-status                  pic x(02) value "00".
       01 cd02-stat                    pic x(01) value "F".
      *
       01 nome-arq-cd02.
          02 cd02-dir                  pic x(03) value "CD1".
          02 filler                    pic x(01) value "\".
          02 cd02-nome                 pic x(08) value "ARQCD02A".
          02 filler                    pic x(01) value ".".
          02 cd02-ext                  pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-cliente                pic x(40) value
          "ABAV - SP ".
          02 cb-nome                   pic x(18) value
          "ABAV - Informatica".
          02 cb-programa               pic x(08) value "INIC001".
          02 cb-versao                 pic x(06) value "v1.00 ".
          02 cb-data                   pic x(08) value spaces.
      *
       01 usuario                      pic x(10) value spaces.
       01 senha                        pic x(10) value spaces.
       01 cont                         pic 9(01) value 0.
       01 opc                          pic s9(02) value 0.
       01 opc-aux                      pic s9(02) value 0.
      *
       01 data-accept                  pic 9(06) value 0.
       01 data-edit redefines data-accept.
          02 edit-ano                  pic 9(02).
          02 edit-mes                  pic 9(02).
          02 edit-dia                  pic 9(02).
      * 
       01 campo-grafico.
          02 filler                    pic x(01) value low-values.
          02 grafico-modo              pic 9(02) comp-5 value 0.
          02 filler                    pic x(01) value low-values.
          02 grafico-arq               pic x(10) value "X.GRF".
          02 filler                    pic x(01) value low-values.
      *
       01 campo-menu.
          02 menu-argum                pic 9(04) comp-5 value 12.
          02 filler                    pic x(01) value low-values.
          02 menu-tam                  pic 9(04) comp-5 value 15.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-f                pic 9(04) comp-5 value 2.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-p                pic 9(04) comp-5 value 6.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-fb               pic 9(04) comp-5 value 5.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-pb               pic 9(04) comp-5 value 15.
          02 filler                    pic x(01) value low-values.
          02 menu-dados.
             03 menu-sentido           pic x(01) value "V".
             03 menu-pos               pic 9(02) comp-5 value 0.
             03 filler                 pic x(01) value x"b3".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 07.
             03 filler                 pic x(15) value " 1-Usuarios".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 08.
             03 filler                 pic x(15) value " 2-Impressoras".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 09.
             03 filler                 pic x(15) value " 3-Tabelas".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 10.
             03 filler                 pic x(15) value " 4-Associados".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic x(15) value " 5-Balcao".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 12.
             03 filler                 pic x(15) value " 6-bilhetes".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 13.
             03 filler                 pic x(15) value " 7-Receber".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 14.
             03 filler                 pic x(15) value " 8-Baixas".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 15.
             03 filler                 pic x(15) value " 9-Titulares".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 16.
             03 filler                 pic x(15) value " A-S.P.T.".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 17.
             03 filler                 pic x(15) value " B-Mala Direta".
             03 filler                 pic 9(02) comp-5 value 30.
             03 filler                 pic 9(02) comp-5 value 18.
             03 filler                 pic x(15) value " C-Contatos".
      *
       copy workgen.lib.
      *
       screen section.
      * 
       01 tela-cabecalho.
          02 line 02 column 03 foreground-color 07 background-color 06
             highlight pic x(40) from cb-cliente.
          02 line 02 column 61 foreground-color 07 background-color 06
             highlight pic x(18) from cb-nome.
          02 line 24 column 01 foreground-color 07 background-color 06
             highlight pic x(80) from spaces.
      *
        01 tela-rodape.
          02 line 24 column 02 foreground-color 07 background-color 06
             highlight pic x(08) from cb-programa.
          02 line 24 column 12 foreground-color 07 background-color 06
             highlight pic x(08) from cb-versao.
          02 line 24 column 30 foreground-color 07 background-color 06
             value "Usr.:".
          02 line 24 column 36 foreground-color 07 background-color 06
             highlight pic x(08) from usuario.
          02 line 24 column 70 foreground-color 07 background-color 06
             highlight pic x(08) from cb-data.         

      *
       01 tela-01.
          02 line 13 column 31 foreground-color 07 background-color 03
             highlight value "Usuario :".
          02 line 13 column 41 foreground-color 05 background-color 01
             pic x(10) from spaces.
          02 line 15 column 31 foreground-color 07 background-color 03
             highlight value "Senha   :".
          02 line 15 column 41 foreground-color 05 background-color 01
             pic x(10) from spaces.
      *
       copy scrgen.lib.
      *
       procedure division.
      *
       lab-00.
      *     move 4 to grafico-modo.
      *     call "C_SetC_modo" using by value grafico-modo.
      *     call "C_MoveC_image" using by reference grafico-arq.
      *     move 0 to box-lin.
      *     move 0 to box-col.
      *     call "C_Gotoxy" using by value box-col
      *                           by value box-lin.
      *     perform rot-keypress.
      *     move 3 to grafico-modo.
      *     call "C_SetC_modo" using by value grafico-modo.
           move 2 to box-cor-f.
           move 5 to box-cor-p.
           move x"dd" to box-fundo.
           perform rot-box.
           move 02 to box-lin-f.
           move 06 to box-cor-f.
           move 07 to box-cor-p.
           move 1 to box-borda.
           move spaces to box-fundo.
           perform rot-box.
           display tela-cabecalho.
           move 28 to box-col.
           move 11 to box-lin.
           move 54 to box-col-f.
           move 17 to box-lin-f.
           perform rot-save-buffer.
           subtract 2 from box-col-f box-lin-f.
           move 03 to box-cor-f.
           move 15 to box-cor-p.
           move "S" to box-sombra.
           perform rot-box.
       lab-01.
           display tela-01.
           move spaces to usuario.
           perform acc-usuario.
           if escape-key = 1
              go to lab-fim.
           if usuario = spaces
              go to lab-01.
           move usuario to txt.
           call "C_Text" using by reference campo-txt.
           move txt to usuario.
           perform dsp-usuario.
           if usuario not = "SU"
              move " Usuario nao cadastrado - Tecle <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              go to lab-01
           end-if.
           move 0 to cont.
           accept data-accept from date.
           move edit-ano to ano-euro.
           move edit-mes to mes-euro.
           move edit-dia to dia-euro.
           move "4" to opcao-data.
           perform rot-data.
           move data-disp to cb-data.
      *
       lab-03.
           add 1 to cont.
           perform lmp-senha.
           perform acc-senha.
           if escape-key = 1
              go to lab-01
           end-if.
           if senha = spaces
              subtract 1 from cont
              go to lab-03
           end-if.
           move senha to txt.
           call "C_Text" using by reference campo-txt.
           if txt not = "KEPLER"
              move " Senha incorreta - Tecle <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              if cont < 3
                 go to lab-03
              else
                 go to lab-fim
              end-if
           end-if.
           add 2 to box-col-f box-lin-f.
           perform rot-rest-buffer.
           display tela-rodape.
           move 29 to box-col.
           move 06 to box-lin.
           move 45 to box-col-f.
           move 19 to box-lin-f.
           move "2" to box-borda.
           move 02 to box-cor-f.
           move 15 to box-cor-p.
           perform rot-box.
           perform until opc = -1 
                   call "C_Menu" using by value menu-argum
                                       by value menu-tam
                                       by value menu-cor-f
                                       by value menu-cor-p
                                       by value menu-cor-fb
                                       by value menu-cor-pb
                                       by reference menu-dados
                   move return-code to opc
                   move 0 to box-col box-lin
                   move 80 to box-col-f
                   move 25 to box-lin-f
                   perform rot-save-buffer
                   evaluate true
                            when opc = 1
                                 perform rot-open-usr
                            when opc = 2
                                 perform rot-open-imp
                            when opc = 3
                                 perform rot-open-tabl
                            when opc = 4
                                 perform rot-open-ab01
                            when opc = 5
                                 perform rot-open-ab02
                            when opc = 6
                                 perform rot-open-ab03
                            when opc = 7
                                 perform rot-open-rc01
                            when opc = 8
                                 perform rot-open-bx01
                            when opc = 9
                                 perform rot-open-ab04
                            when opc = 10
                                 perform rot-open-sl01
                            when opc = 11
                                 perform rot-open-cd01
                            when opc = 12
                                 perform rot-open-cd02
                   end-evaluate
                   move 0 to box-col box-lin
                   move 80 to box-col-f
                   move 25 to box-lin-f
                   if opc not = -1
                      perform rot-rest-buffer
                      display tela-rodape
                   end-if
           end-perform.

      *
       lab-fim.
           call "C_Cls".
           move 01 to box-lin box-col
           call "C_Gotoxy" using by value box-col
                                 by value box-lin.
           stop run.
      *
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *      
       rotina section.
      *
       rot-open-usr.
           open output arqusr.
           move zeros to reg-usr.
           move high-values to usr-chave.
           write reg-usr.
           move "SU" to usr-usuario
           move spaces to campo-cript.
           move "kepler" to cript-txt.
           move 1 to cript-opcao.
           move 10 to cript-tam.
           call "C_Criptxt" using by value cript-opcao
                                  by value cript-tam
                                  by reference campo-cript.
           move cript-txt to usr-senha
           move 9 to usr-prioridade
           write reg-usr
           close arqusr.
      *
       rot-open-imp.
           open output arqimp.
           move zeros to reg-imp.
           move high-values to imp-chave.
           write reg-imp.
           close arqimp.
      *
       rot-open-ab01.
           open output arqab01.
           move zeros to reg-ab01.
           move high-values to ab01-chave-controle.
           write reg-ab01.
           close arqab01.
      *
       rot-open-ab02.
           open output arqab02.
           move zeros to reg-ab02.
           move high-values to ab02-chave-controle.
           write reg-ab02.
           close arqab02.
      *
       rot-open-ab03.
           open output arqab03.
           move zeros to reg-ab03.
           move high-values to ab03-chave-controle.
           write reg-ab03.
           close arqab03.
      *
       rot-open-rc01.
           open output arqrc01.
           move zeros to reg-rc01-1.
           move high-values to rc01-controle.
           move 0 to rc01-ult-fat.
           write reg-rc01-1.
      *
       rot-open-bx01.
           open output arqbx01.
           move zeros to reg-bx01-1.
           move high-values to bx01-controle.
           move 0 to bx01-ult-bx.
           write reg-bx01-1.
      *
       rot-open-tabl.
           open output arqtabl.
           move zeros to reg-tabl.
           move high-values to tabl-chave tabl-chave-1.
           write reg-tabl.
           close arqtabl.
      *
       rot-open-ab04.
           open output arqab04.
           move zeros to reg-ab04.
           move high-values to ab04-chave-controle.
           write reg-ab04.
           close arqab04.
      *
       rot-open-sl01.
           open output arqsl01.
           move zeros to reg-sl01.
           move high-values to sl01-chave-controle.
           write reg-sl01.
           close arqsl01.
      *
       rot-open-cd01.
           open output arqcd01.
           move zeros to reg-cd01.
           move high-values to cd01-chave-controle.
           write reg-cd01.
           close arqcd01.
      *
       rot-open-cd02.
           open output arqcd02.
           move zeros to reg-cd02.
           move high-values to cd02-chave-controle.
           write reg-cd02.
           close arqcd02.
      *
       copy rotgen.lib.
      *
      ************************
      *                      *
      *       T e l a s      *
      *                      *
      ************************
      *
       telas section.
      *
       acc-usuario.
           accept usuario at 1341 with auto update foreground-color 10
                                       background-color 01.
           accept escape-key from escape.
           exit.
      *
       dsp-usuario.
           display usuario at 1341 with foreground-color 10
                                        background-color 01.
      *
       acc-senha.
           accept senha at 1541 with auto secure foreground-color 10
                                     background-color 01.
           accept escape-key from escape.
           exit.
      *
       lmp-senha.
           move spaces to senha.
           display senha at 1541 with foreground-color 10
                                      background-color 01.
      *
