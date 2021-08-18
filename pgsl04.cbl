      ***************************************************************
      *                                                             *
      *  A B A V / S P -  I N F O R M A T I C A   :::  PGSL04       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Impressao de Etiquetas - S.P.T.:                           *
      *                                                             *
      *  Data da ultima alteracao:    07/04/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgsl04.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqsl01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is automatic
                  with lock on record
                  record key is sl01-chave
                  alternate record key is sl01-chave-1 with duplicates
                  alternate record key is sl01-chave-2 with duplicates
                  alternate record key is sl01-chave-3 with duplicates
                  alternate record key is sl01-chave-4 with duplicates
                  file status is sl01-status.
      *
           select arqimp assign to printer
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdsl01.lib.
      *
       fd arqimp

       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *
       working-storage section.
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
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGSL04".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(10) value spaces.
       01 limpa-aux                    pic x(15) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 campo-dorme                  pic 9(04) comp-5 value 3.
       01 chave-ant                    pic 9(05) value 0.
       01 sequencia                    pic 9(01) value 1.
       01 linha                        pic 9(03) comp-5 value 0.
      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
          02 sele-tipo                 pic 9(01) value 0.
          02 sele-grupo                pic x(01) value spaces.
          02 sele-dgrupo               pic x(05) value spaces.
          02 sele-uf                   pic x(02) value spaces.
          02 sele-duf                  pic x(05) value spaces.
          02 sele-presenca             pic x(01) value spaces.
          02 sele-dpresenca            pic x(05) value spaces.
      *
       01 cab-01.
          02 filler                    pic x(01) value spaces.
          02 cab-empresa-1             pic x(38) value spaces.
          02 filler                    pic x(06) value spaces.
          02 cab-empresa-2             pic x(38) value spaces.
      *
       01 cab-02.
          02 filler                    pic x(01) value spaces.
          02 cab-ac-1                  pic x(03) value "A/C".
          02 filler                    pic x(01) value spaces.
          02 cab-nome-1                pic x(34) value spaces.
          02 filler                    pic x(06) value spaces.
          02 cab-ac-2                  pic x(03) value "A/C".
          02 filler                    pic x(01) value spaces.
          02 cab-nome-2                pic x(34) value spaces.
      *
       01 cab-03.
          02 filler                    pic x(01) value spaces.
          02 cab-endereco-1            pic x(38) value spaces.
          02 filler                    pic x(06) value spaces.
          02 cab-endereco-2            pic x(38) value spaces.
      *
       01 cab-04.
          02 filler                    pic x(01) value spaces.
          02 cab-cep-1                 pic 9(05)b9(03) value 0.
          02 filler                    pic x(02) value spaces.
          02 cab-cidade-1              pic x(15) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-uf-1                  pic x(02) value spaces.
          02 filler                    pic x(14) value spaces.
          02 cab-cep-2                 pic 9(05)b9(03) value 0.
          02 filler                    pic x(02) value spaces.
          02 cab-cidade-2              pic x(15) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-uf-2                  pic x(02) value spaces.
      *
       01 cab-05.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "".
          02 cab-nome-c                pic x(16) value spaces.
          02 filler                    pic x(01) value "".
          02 filler                    pic x(12) value spaces.
          02 cab-empresa-e             pic x(35) value spaces.
      *
       01 cab-06.
          02 filler                    pic x(45) value spaces.
          02 cab-ac-e                  pic x(03) value "A/C".
          02 filler                    pic x(01) value spaces.
          02 cab-nome-e                pic x(31) value spaces.
      *
       01 cab-07.
          02 filler                    pic x(01) value spaces.
          02 cab-empresa-c             pic x(38) value spaces.
          02 filler                    pic x(06) value spaces.
          02 cab-endereco-e            pic x(35) value spaces.
      *
       01 cab-08.
          02 filler                    pic x(01) value spaces.
          02 cab-cidade-c              pic x(15) value spaces.
          02 filler                    pic x(05) value spaces.
          02 cab-uf-c                  pic x(02) value spaces.
          02 filler                    pic x(22) value spaces.
          02 cab-cep-e                 pic 9(05)b9(03) value 0
             blank when zero.
          02 filler                    pic x(02) value spaces.
          02 cab-cidade-e              pic x(15) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-uf-e                  pic x(02) value spaces.
      *
       01 cab-09.
          02 filler                    pic x(40) value spaces.
          02 cab-grupo-c               pic x(01) value spaces.
      *
       copy workgen.lib.
      * 
       linkage section.
      *
       01 param-menu.
          02 param-usr                 pic x(10).
          02 param-senha               pic x(10).
          02 param-prioridade          pic 9(01).
          02 param-data                pic 9(05).
          02 param-impress             pic x(12).
      *
       screen section.
      *
       01 tela-01.
          02 line 13 column 29 foreground-color 06 background-color 01
             highlight value "Etiquetas de Participantes".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Ordenamento...:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Tipo..........:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Grupo.........:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "U.F...........:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "Presenca......:".
      *
       01 tela-02.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(50) from spaces.
          02 line 20 column 07 foreground-color 02 background-color 03
             highlight value "1".
          02 line 20 column 08 foreground-color 05 background-color 03
             value "-Codigo".
          02 line 20 column 18 foreground-color 02 background-color 03
             highlight value "2".
          02 line 20 column 19 foreground-color 05 background-color 03
             value "-Empresa".
          02 line 20 column 31 foreground-color 02 background-color 03
             highlight value "3".
          02 line 20 column 32 foreground-color 05 background-color 03
             value "-Nome".
          02 line 20 column 40 foreground-color 02 background-color 03
             highlight value "4".
          02 line 20 column 41 foreground-color 05 background-color 03
             value "-Grupo".
          02 line 20 column 50 foreground-color 02 background-color 03
             highlight value "5".
          02 line 20 column 51 foreground-color 05 background-color 03
             value "-uf".
      *
       01 tela-03.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(50) from spaces.
          02 line 20 column 08 foreground-color 02 background-color 03
             highlight value "S".
          02 line 20 column 09 foreground-color 05 background-color 03
             value " - Sim".
          02 line 20 column 25 foreground-color 02 background-color 03
             highlight value "N".
          02 line 20 column 26 foreground-color 05 background-color 03
             value " - Nao".
      *
       01 tela-04.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(50) from spaces.
          02 line 20 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 20 column 09 foreground-color 05 background-color 03
             value "-Cracha".
          02 line 20 column 25 foreground-color 02 background-color 03
             highlight value "2".
          02 line 20 column 26 foreground-color 05 background-color 03
             value "-Endereco".
      *
       01 tela-05.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(50) from spaces.
          02 line 20 column 07 foreground-color 02 background-color 03
             highlight value "O".
          02 line 20 column 08 foreground-color 05 background-color 03
             value "-Org".
          02 line 20 column 15 foreground-color 02 background-color 03
             highlight value "I".
          02 line 20 column 16 foreground-color 05 background-color 03
             value "-Imp".
          02 line 20 column 23 foreground-color 02 background-color 03
             highlight value "V".
          02 line 20 column 24 foreground-color 05 background-color 03
             value "-Vis".
          02 line 20 column 31 foreground-color 02 background-color 03
             highlight value "A".
          02 line 20 column 32 foreground-color 05 background-color 03
             value "-Age".
          02 line 20 column 39 foreground-color 02 background-color 03
             highlight value "F".
          02 line 20 column 40 foreground-color 05 background-color 03
             value "-Prof".
          02 line 20 column 48 foreground-color 02 background-color 03
             highlight value "M".
          02 line 20 column 49 foreground-color 05 background-color 03
             value "-Prom".
      *
       01 tela-06.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(50) from spaces.
          02 line 20 column 06 foreground-color 05 background-color 03
             value "(".
          02 line 20 column 07 foreground-color 02 background-color 03
             highlight value "I".
          02 line 20 column 08 foreground-color 05 background-color 03
             value ")mprimir   (".
          02 line 20 column 20 foreground-color 02 background-color 03
             highlight value "P".
          02 line 20 column 21 foreground-color 05 background-color 03
             value ")osicionar   (".
          02 line 20 column 35 foreground-color 02 background-color 03
             highlight value "E".
          02 line 20 column 36 foreground-color 05 background-color 03
             value ")ncerrar".
      *
       01 tela-08.
          02 line 20 column 05 foreground-color 05 background-color 03
             pic x(50) from spaces.
          02 line 20 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 20 column 12 foreground-color 02 background-color 03
             highlight value "C".
          02 line 20 column 13 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 20 column 26 foreground-color 02 background-color 03
             highlight value "F".
          02 line 20 column 27 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-09.
          02 line 20 column 05 foreground-color 05 background-color 03
             pic x(50) from spaces.
          02 line 20 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 20 column 12 foreground-color 02 background-color 03
             highlight value "I".
          02 line 20 column 13 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-mensagem-cad.
          02 line 20 column 05 foreground-color 07 background-color 01
             highlight pic x(50) from mensagem.
      *
       01 tela-erro-cad.
          02 line 20 column 05 beep reverse-video pic x(50) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 20 column 05 foreground-color 01 background-color 01
             pic x(50) from spaces.
      *
       copy scrgen.lib.
      *
       procedure division using param-menu.
      *
       lab-00.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-save-buffer.
           display tela-cabec.
           move 03 to box-col.
           move 11 to box-lin.
           move 54 to box-col-f.
           move 20 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-01.
      *
       lab-01.
           display tela-limpa-cad.
           perform sec-selecao.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-rest-buffer.
      *
       lab-fim.
           exit program.
      *
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *      
       rotinas section.
      *
       rot-move.
           if sequencia = 1
              move sl01-empresa-a to cab-empresa-1
              move sl01-nome-a to cab-nome-1
              move sl01-endereco to cab-endereco-1
              move sl01-cep to cab-cep-1
              move sl01-cidade to cab-cidade-1
              move sl01-uf to cab-uf-1
           else
              if sequencia = 2
                 move sl01-empresa-a to cab-empresa-2
                 move sl01-nome-a to cab-nome-2
                 move sl01-endereco to cab-endereco-2
                 move sl01-cep to cab-cep-2
                 move sl01-cidade to cab-cidade-2
                 move sl01-uf to cab-uf-2
              else
                 move sl01-empresa-a to cab-empresa-c
                 move sl01-nome-cracha to cab-nome-c
                 move sl01-grupo to cab-grupo-c
                 move sl01-uf to cab-uf-c
                 move sl01-cidade to cab-cidade-c
      *           if sl01-grupo = "A"
                    move sl01-empresa-a to cab-empresa-e
                    move sl01-nome-a to cab-nome-e
                    move sl01-endereco to cab-endereco-e
                    move sl01-cep to cab-cep-e
                    move sl01-cidade to cab-cidade-e
                    move sl01-uf to cab-uf-e
                    move "A/C" to cab-ac-e
      *           else
      *              move spaces to cab-empresa-e
      *              move spaces to cab-nome-e
      *              move spaces to cab-endereco-e
      *              move 0 to cab-cep-e
      *              move spaces to cab-cidade-e
      *              move spaces to cab-uf-e
      *              move spaces to cab-ac-e
      *           end-if
              end-if
           end-if.
      *
       rot-posicionar.
           if sele-tipo = 2
              move all "*" to cab-empresa-1 cab-empresa-2
              move all "*" to cab-nome-1 cab-nome-2
              move all "*" to cab-endereco-1 cab-endereco-2
              move 99999999 to cab-cep-1 cab-cep-2
              move all "*" to cab-cidade-1 cab-cidade-2
              move all "*" to cab-uf-1 cab-uf-2
              write reg-imp from cab-01 after 0 line
              write reg-imp from cab-02 after 1 line
              write reg-imp from cab-03 after 1 line
              write reg-imp from cab-04 after 1 line
              write reg-imp from spaces after 3 line
           else
              move all "*" to cab-empresa-c
              move all "*" to cab-nome-c
              move all "*" to cab-grupo-c
              move all "*" to cab-uf-c
              move all "*" to cab-cidade-c
              move all "*" to cab-empresa-e
              move all "*" to cab-nome-e
              move all "*" to cab-endereco-e
              move sl01-cep to cab-cep-e
              move all "*" to cab-cidade-e
              move all "*" to cab-uf-e
              move "A/C" to cab-ac-e
              write reg-imp from cab-05 after 0 line
              write reg-imp from cab-06 after 1 line
              write reg-imp from cab-07 after 1 line
              write reg-imp from cab-08 after 1 line
              write reg-imp from cab-09 after 1 line
              write reg-imp from spaces after 2 line
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqsl01 next at end move 1 to erro.
           if sl01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-sl01.
           move 0 to erro.
           if sl01-stat = "F"
              open i-o arqsl01
              if sl01-status not = "00"
                 move 
                 " Erro de abertura no ARQSL01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to sl01-stat
               end-if
           end-if.
      *
       rot-close-sl01.
           if sl01-stat = "A"
              close arqsl01
              move "F" to sl01-stat
           end-if.
      *
       err-leitura-sl01.
           move " Erro de leitura - ARQSL01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-open-imp.
           move 0 to erro.
           move param-impress to impress.
           move zeros to imp-status
           if imp-stat = "F"
              open output arqimp
              if imp-status not = "00"
                 move " Erro de impressao - Tecle <Enter>" to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
              else
                 move "A" to imp-stat
              end-if
           end-if.
      *
       rot-close-imp.
           if imp-stat = "A"
              close arqimp 
              unlock arqimp
              move "F" to imp-stat
           end-if.
      *
       rot-interrompe.
           call "C_Readkey".
           move return-code to campo-kbd.
           if kbd2 = 73 or 105
              display tela-08
              perform until kbd2 = 67 or 99 or 70 or 102
                      perform rot-keypress
              end-perform
             if kbd2 = 70 or 102
                move "F" to resposta
             else
                display tela-09
             end-if
           end-if.
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
       accept-resposta-cad.
           move spaces to resposta.
           accept resposta at 1953 with auto foreground-color 01
                                             background-color 01.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
      *  Sequencia para dar Accept
      *
       acc-ord.
           accept sele-ord at 1422 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-tipo.
           accept sele-tipo at 1522 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-grupo.
           accept sele-grupo at 1622 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept sele-uf at 1722 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-presenca.
           accept sele-presenca at 1822 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 1422 with foreground-color 15 
                   background-color 01.
      *
       dsp-tipo.
           display sele-tipo at 1522 with foreground-color 15 
                   background-color 01.
      *
       dsp-grupo.
           display sele-dgrupo at 1622 with foreground-color 15 
                   background-color 01.
      *
       dsp-uf.
           display sele-duf at 1722 with foreground-color 15 
                   background-color 01.
      *
       dsp-presenca.
           display sele-dpresenca at 1822 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1422 with foreground-color 15 
                   background-color 01.
      *
       lmp-tipo.
           display limpa at 1522 with foreground-color 15 
                   background-color 01.
      *
       lmp-grupo.
           display limpa at 1622 with foreground-color 15 
                   background-color 01.
      *
       lmp-uf.
           display limpa at 1722 with foreground-color 15 
                   background-color 01.
      *
       lmp-presenca.
           display limpa at 1822 with foreground-color 15 
                   background-color 01.
      *
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
       sec-selecao section.
      *
       lab-sele-00.
           perform rot-open-sl01.
           if erro not = 0
              go to lab-sele-fim
           end-if.
           display tela-limpa-cad.
           if param-prioridade < 1
              perform display-erro-usr
              go to lab-sele-fim
           end-if.
      *
       lab-sele-01.
           display tela-02.
           move 0 to sele-ord.
           perform lmp-ord.
           perform acc-ord.
           if escape-key = 1
              go to lab-sele-fim
           end-if.
           if sele-ord not = 1 and 2 and 3 and 4 and 5
              go to lab-sele-01
           end-if.
      *
       lab-sele-02.
           display tela-04.
           move 0 to sele-tipo.
           perform lmp-tipo.
           perform acc-tipo.
           if escape-key = 1
              perform lmp-tipo
              go to lab-sele-01
           end-if.
           if sele-tipo not = 1 and 2
              perform lmp-tipo
              go to lab-sele-02
           end-if.
      *
       lab-sele-03.
           display tela-05.
           move spaces to sele-grupo.
           perform lmp-grupo.
           perform acc-grupo.
           if escape-key = 1
              perform lmp-grupo
              go to lab-sele-02
           end-if.
           move sele-grupo to txt.
           perform rot-texto.
           if txt not = space and "A" and "V" and "I" and "F" and
                        "M" and "O" 
              go to lab-sele-03
           end-if.
           move txt to sele-grupo.
           if sele-grupo = spaces
              move "Todos" to sele-dgrupo
           else
              move txt to sele-dgrupo
           end-if.
           perform dsp-grupo.
           display tela-limpa-cad.
      *
       lab-sele-04.
           move spaces to sele-uf.
           perform lmp-uf.
           perform acc-uf.
           if escape-key = 1
              perform lmp-uf
              go to lab-sele-03
           end-if.
           move sele-uf to txt. 
           perform rot-texto.
           move txt to sele-uf.
           if sele-uf = spaces
              move "Todos" to sele-duf
           else
              move txt to sele-duf
           end-if.
           perform dsp-uf.
      *
       lab-sele-05.
           display tela-03.
           move spaces to sele-presenca.
           perform lmp-presenca.
           perform acc-presenca.
           if escape-key = 1
              perform lmp-presenca
              display tela-limpa-cad
              go to lab-sele-04
           end-if.
           move sele-presenca to txt.
           perform rot-texto.
           if txt not = space and "S" and "N" 
              go to lab-sele-05
           end-if.
           move txt to sele-presenca.
           if sele-presenca = spaces
              move "Todos" to sele-dpresenca
           else
              move txt to sele-dpresenca
           end-if.
           perform dsp-presenca.
      *
       lab-sele-06.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-05
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-ord thru lmp-presenca
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-06
              end-if
           end-if.
           display tela-limpa-cad.
           perform sec-impressao
           perform lmp-ord thru lmp-presenca.
           display tela-limpa-cad.
           go to lab-sele-01.
      *
       lab-sele-fim.
           exit.
      *
       sec-impressao section.
       lab-imp-00.
           perform rot-open-sl01.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           display tela-06.
           move zeros to campo-kbd.
           perform until kbd2 = 27 or 73 or 105 or 69 or 101
                   perform rot-keypress
                   if kbd2 = 80 or 112
                      perform rot-posicionar
                   end-if
           end-perform.
           if kbd2 = 27 or 69 or 101
              go to lab-imp-fim
           end-if.
           if sele-tipo = 1
              move 0 to sequencia
           else
              move 1 to sequencia
           end-if.
           evaluate true
                  when sele-ord = 1
                       move low-values to sl01-chave
                       start arqsl01 key is not less sl01-chave
                  when sele-ord = 2
                       move low-values to sl01-chave-1
                       start arqsl01 key is not less sl01-chave-1
                  when sele-ord = 3
                       move low-values to sl01-chave-2
                       start arqsl01 key is not less sl01-chave-2
                  when sele-ord = 4
                       move low-values to sl01-chave-3
                       start arqsl01 key is not less sl01-chave-3
                  when sele-ord = 5
                       move low-values to sl01-chave-4
                       start arqsl01 key is not less sl01-chave-4
           end-evaluate.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if sl01-chave = high-values
              if sequencia = 2
                 move spaces to cab-ac-2
                 write reg-imp from cab-01 after 0 line
                 write reg-imp from cab-02 after 1 line
                 write reg-imp from cab-03 after 1 line
                 write reg-imp from cab-04 after 1 line
                 write reg-imp from spaces after 3 line 
              end-if
              go to lab-imp-01
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if sele-grupo not = spaces
              if sele-grupo not = sl01-grupo
                 go to lab-imp-01
              end-if
           end-if.
           if sele-uf not = spaces
              if sele-uf not = sl01-uf
                 go to lab-imp-01
              end-if
           end-if.
           if sele-presenca not = spaces
              if sele-presenca not = sl01-presenca
                 go to lab-imp-01
              end-if
           end-if.
           perform rot-move.
           if sele-tipo = 2 and sequencia = 1
              move 2 to sequencia
              go to lab-imp-01
           end-if.
           if sele-tipo = 1
              if sl01-etiqueta = "S"
                 go to lab-imp-01
              end-if
              write reg-imp from cab-05 after 0 line
              write reg-imp from cab-06 after 1 line
              write reg-imp from cab-07 after 1 line
              write reg-imp from cab-08 after 1 line
              write reg-imp from cab-09 after 1 line
              write reg-imp from spaces after 2 line
              move "S" to sl01-etiqueta
              rewrite reg-sl01 invalid key
                      move 
                      " Erro de regravacao no ARQSL01A.DAT - Tecle <Ente
      -               "r>"
                      to mensagem
                      display tela-erro
                      perform rot-keypress
                      display tela-limpa
                      go to lab-imp-fim
              end-rewrite
           else
              write reg-imp from cab-01 after 0 line
              write reg-imp from cab-02 after 1 line
              write reg-imp from cab-03 after 1 line
              write reg-imp from cab-04 after 1 line
              write reg-imp from spaces after 3 line
              move 1 to sequencia
           end-if.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           move zeros to campo-kbd.
           perform rot-close-imp.
           perform rot-close-sl01.
           exit.
      *