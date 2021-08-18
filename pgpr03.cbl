      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGPR03       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Etiquetas do cadastro :                                    *
      *                                                             *
      *  Data da ultima alteracao:    10/01/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgpr03.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqat01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is at01-chave
                  alternate record key is at01-chave-1 with duplicates
                  alternate record key is at01-chave-2 with duplicates
                  file status is at01-status.
      *
           select arqat02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is at02-chave
                  file status is at02-status.
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
           select arqimp assign to disk
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdat01.lib.
      *
       copy fdat02.lib.
      *
       copy fdtabl.lib.
      *
       fd arqimp
      
       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *
       working-storage section.
      *
       01 at01-status                  pic x(02) value "00".
       01 at01-stat                    pic x(01) value "F".
      *
       01 nome-arq-at01.
          02 at01-dir                  pic x(03) value "AT2".
          02 filler                    pic x(01) value "\".
          02 at01-nome                 pic x(08) value "ARQAT01A".
          02 filler                    pic x(01) value ".".
          02 at01-ext                  pic x(03) value "DAT".
      *
       01 at02-status                  pic x(02) value "00".
       01 at02-stat                    pic x(01) value "F".
      *
       01 nome-arq-at02.
          02 at02-dir                  pic x(03) value "AT2".
          02 filler                    pic x(01) value "\".
          02 at02-nome                 pic x(08) value "ARQAT02A".
          02 filler                    pic x(01) value ".".
          02 at02-ext                  pic x(03) value "DAT".
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
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGPR03".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 limpa                        pic x(15) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 linha                        pic 9(03) comp-5 value 0.
       01 coluna                       pic 9(04) comp-5 value 1.
       01 tamanho                      pic 9(04) comp-5 value 78.
       01 pagina                       pic 9(03) value 0.
       01 sequencia                    pic 9(05) value 0.
       01 sub                          pic 9(03) value 0.
       01 sub1                         pic 9(03) value 0.
       01 tot-lidos                    pic 9(04) value 0.
       01 tot-impressos                pic 9(04) value 0.
      *
       01 campos.
          02 sele-uf                   pic x(02) value spaces.
          02 sele-uf-disp              pic x(05) value spaces.
          02 codigo                    pic x(06) value spaces.
          02 empresa-a                 pic x(40) value spaces.
          02 diretor-a                 pic x(40) value spaces.
          02 endereco                  pic x(40) value spaces.
          02 cep                       pic x(08) value spaces.
          02 cidade                    pic x(15) value spaces.
          02 uf                        pic x(02) value spaces.
      *
       01 tabela.
          02 tab-reg                   pic x(01) occurs 150.
      *
       01 tabela-aux.
          02 tab-reg-aux               pic x(01) occurs 55.
      *
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 campo-rotina-uf.
          02 rotina-col-uf             pic 9(02) value 0.
          02 rotina-lin-uf             pic 9(02) value 0.
          02 rotina-borda-uf           pic x(01) value spaces.
          02 rotina-fundo-uf           pic x(01) value spaces.
          02 rotina-sombra-uf          pic x(01) value spaces.
          02 rotina-tipo-uf            pic 9(02) value 03.
          02 rotina-uf                 pic x(02) value spaces.
      *
       01 cab-02.
          02 cab-razao-1               pic x(40) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-razao-2               pic x(40) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-razao-3               pic x(40) value spaces.
      *
       01 cab-03.
          02 cab-ac-1                  pic x(03) value spaces.
          02 filler                    pic x(01) value spaces.
          02 cab-titular-1             pic x(36) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-ac-2                  pic x(03) value spaces.
          02 filler                    pic x(01) value spaces.
          02 cab-titular-2             pic x(36) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-ac-3                  pic x(03) value spaces.
          02 filler                    pic x(01) value spaces.
          02 cab-titular-3             pic x(36) value spaces.
      *
       01 cab-04.
          02 cab-endereco-1            pic x(40) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-endereco-2            pic x(40) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-endereco-3            pic x(40) value spaces.
      *
       01 cab-05.
          02 cab-cep-1                 pic 99999b999 value 0.
          02 filler                    pic x(05) value spaces.
          02 cab-cidade-1              pic x(20) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-uf-1                  pic x(02) value spaces.
          02 filler                    pic x(05) value spaces.
          02 cab-cep-2                 pic 99999b999 value 0.
          02 filler                    pic x(05) value spaces.
          02 cab-cidade-2              pic x(20) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-uf-2                  pic x(02) value spaces.
          02 filler                    pic x(05) value spaces.
          02 cab-cep-3                 pic 99999b999 value 0.
          02 filler                    pic x(05) value spaces.
          02 cab-cidade-3              pic x(20) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-uf-3                  pic x(02) value spaces.
      *
       copy workgen.lib.
       copy wstab03.lib.
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
          02 line 13 column 09 foreground-color 07 background-color 04
             highlight value "Etiquetas de Agencias nao cadastradas".
          02 line 15 column 06 foreground-color 06 background-color 04
             value "UF..................:".
          02 line 17 column 06 foreground-color 06 background-color 04
             value "Registros Lidos.....:".
          02 line 18 column 06 foreground-color 06 background-color 04
             value "Registros Impressos.:".
      *
       01 tela-03.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 20 column 06 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 20 column 08 foreground-color 01 background-color 02
             value " - Estados".
      *
       01 tela-07.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 20 column 06 foreground-color 01 background-color 02
             value "(".
          02 line 20 column 07 foreground-color 07 background-color 02
             highlight value "I".
          02 line 20 column 08 foreground-color 01 background-color 02
             value ")mprimir   (".
          02 line 20 column 20 foreground-color 07 background-color 02
             highlight value "P".
          02 line 20 column 21 foreground-color 01 background-color 02
             value ")osicionar   (".
          02 line 20 column 35 foreground-color 07 background-color 02
             highlight value "E".
          02 line 20 column 36 foreground-color 01 background-color 02
             value ")ncerrar".
      *
       01 tela-08.
          02 line 20 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 20 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 20 column 12 foreground-color 07 background-color 02
             highlight value "C".
          02 line 20 column 13 foreground-color 01 background-color 02
             value ")ontinuar   (".
          02 line 20 column 26 foreground-color 07 background-color 02
             highlight value "F".
          02 line 20 column 27 foreground-color 01 background-color 02
             value ")inalizar".
      *
       01 tela-09.
          02 line 20 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 20 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 20 column 12 foreground-color 07 background-color 02
             highlight value "I".
          02 line 20 column 13 foreground-color 01 background-color 02
             value ") para interromper".
      *
       01 tela-mensagem-cad.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from mensagem.
      *
       01 tela-erro-cad.
          02 line 20 column 05 beep reverse-video pic x(41) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 20 column 05 foreground-color 04 background-color 04
             pic x(41) from spaces.
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
           move 45 to box-col-f.
           move 20 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
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
       rot-le-proximo.
           move 0 to erro.
           read arqat01 next at end move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-at01.
           move 0 to erro.
           if at01-stat = "F"
              open i-o arqat01
              if at01-status not = "00"
                 move 
                 " Erro de abertura no ARQAT01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to at01-stat
               end-if
           end-if.
      *
       rot-close-at01.
           if at01-stat = "A"
              close arqat01
              move "F" to at01-stat
           end-if.
      *
       err-leitura-at01.
           move " Erro de leitura - ARQAT01A.DAT - Tecle <Enter>" to
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
       rot-erro-leitura-at01.
           move " Erro de leitura - ARQAT01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-open-tabl.
           move 0 to erro.
           if tabl-stat = "F"
              open i-o arqtabl
              if tabl-status not = "00"
                 move 
                 " Erro de abertura no ARQTABLA.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
              else
                 move "A" to tabl-stat
              end-if
           end-if.
      *
       rot-close-tabl.
           if tabl-stat = "A"
              close arqtabl
              move "F" to tabl-stat
           end-if.
      *
       rot-le-tabl.
           move 0 to erro.
           read arqtabl invalid key move 1 to erro.
           if tabl-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-tabl
           end-if.
      *
       rot-open-at02.
           move 0 to erro.
           if at02-stat = "F"
              open i-o arqat02
              if at02-status not = "00"
                 move 
                 " Erro de abertura no ARQAT02A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
                 move zeros to reg-at02
                 move high-values to at02-controle
                 write reg-at02-1
               else
                  move "A" to at01-stat
               end-if
           end-if.
      *
       rot-close-at02.
           if at02-stat = "A"
              close arqat02
              move "F" to at02-stat
           end-if.
      *
       rot-le-at02.
           move 0 to erro.
           read arqat02 invalid key move 1 to erro.
           if at02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-at02
           end-if.
      *
       rot-erro-leitura-at02.
           move " Erro de leitura - ARQAT02A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-interrompe-impressora.
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
       rot-move.
           add 1 to sequencia
           evaluate true
                    when sequencia = 1
                         move at01-empresa-a to cab-razao-1
                         move "A/C" to cab-ac-1
                         move at01-diretor-a to cab-titular-1
                         move at01-endereco to cab-endereco-1
                         move at01-cep to cab-cep-1
                         move at01-cidade to cab-cidade-1
                         move at01-uf to cab-uf-1
                         display "**     " at 1833
                    when sequencia = 2
                         move at01-empresa-a to cab-razao-2
                         move "A/C" to cab-ac-2
                         move at01-diretor-a to cab-titular-2
                         move at01-endereco to cab-endereco-2
                         move at01-cep to cab-cep-2
                         move at01-cidade to cab-cidade-2
                         move at01-uf to cab-uf-2
                         display "****   " at 1833
                    when sequencia = 3
                         move at01-empresa-a to cab-razao-3
                         move "A/C" to cab-ac-3
                         move at01-diretor-a to cab-titular-3
                         move at01-endereco to cab-endereco-3
                         move at01-cep to cab-cep-3
                         move at01-cidade to cab-cidade-3
                         move at01-uf to cab-uf-3
                         display "******" at 1833
           end-evaluate.
      *
       rot-posicionar.
           move all "*" to cab-razao-1 cab-razao-2 cab-razao-3.
           move "A/C" to cab-ac-1 cab-ac-2 cab-ac-3.
           move all "*" to cab-titular-1 cab-titular-2 cab-titular-3.
           move all "*" to cab-endereco-1 cab-endereco-2 cab-endereco-3.
           move 99999999 to cab-cep-1 cab-cep-2 cab-cep-3.
           move all "*" to cab-cidade-1 cab-cidade-2 cab-cidade-3.
           move all "*" to cab-uf-1 cab-uf-2 cab-uf-3.
           write reg-imp from spaces after 0 line.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-03 after 1 line.
           write reg-imp from cab-04 after 1 line.
           write reg-imp from cab-05 after 1 line.
           write reg-imp from spaces after 2 line.
      *
       rot-pesq-tabela-3.
           perform rot-close-tabl.
           move 08 to rotina-col-uf.
           move 08 to rotina-lin-uf.
           move "3" to rotina-borda-uf.
           move spaces to rotina-fundo-uf.
           move "S" to rotina-sombra-uf.
           call "pgtab02" using param-menu campo-rotina-uf.
           cancel "pgtab02".
           perform rot-open-tabl.
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
       err-estado.
            move " Estado nao cadastrado - Tecle <Enter>" to mensagem.
            display tela-erro-cad.
            perform rot-keypress.
            display tela-03.
      *
       accept-resposta-cad.
           move spaces to resposta.
           accept resposta at 1740 with auto foreground-color 04
                                             background-color 04.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
      *  Sequencia para dar Accept
      *
       acc-uf.
           accept sele-uf at 1528 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-uf.
           display sele-uf-disp at 1528 with foreground-color 15 
                   background-color 04.
      *
       dsp-tot-lidos.
           display tot-lidos at 1728 with foreground-color 15 
                   background-color 04.
      *
       dsp-tot-impressos.
           display tot-impressos at 1828 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-uf.
           display limpa at 1528 with foreground-color 15 
                   background-color 04.
      *
       lmp-tot-lidos.
           display limpa at 1728 with foreground-color 15 
                   background-color 04.
      *
       lmp-tot-impressos.
           display limpa at 1828 with foreground-color 15 
                   background-color 04.
      *
       sec-selecao section.
      *
       lab-sele-00.
           perform rot-open-tabl.
           if erro not = 0
              go to lab-sele-fim
           end-if.
           move spaces to rotina-uf.
      *
       lab-sele-01.
           display tela-03.
           move spaces to sele-uf-disp.
           move rotina-uf to sele-uf.
           perform lmp-uf.
           perform acc-uf.
           if escape-key = 1
              perform lmp-uf
              go to lab-sele-fim
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-sele-01
           end-if.           
           move sele-uf to txt.
           perform rot-texto.
           move txt to sele-uf.
           if txt = spaces
              move "Todos" to sele-uf-disp 
              perform dsp-uf
              go to lab-sele-02
           end-if.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move sele-uf to wtab03-sigla sele-uf-disp.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado
              go to lab-sele-01
           end-if.
           move reg-tabl to reg-wtab03.
           perform dsp-uf.
           display tela-limpa-cad.
      *
       lab-sele-02.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-01
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-uf
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-02
              end-if
           end-if.
           display tela-limpa-cad.
           perform sec-impressao.
           move spaces to rotina-uf sele-uf.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform rot-close-tabl.
           exit.
      *
       sec-impressao section.
       lab-imp-00.
           perform rot-open-at01.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-at02.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           display tela-07.
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
           move 0 to sequencia tot-lidos tot-impressos.
           perform dsp-tot-lidos thru dsp-tot-impressos.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if at01-chave = high-values
              go to lab-imp-01
           end-if.
           add 1 to tot-lidos.
           perform dsp-tot-lidos.
           perform rot-interrompe-impressora.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if sele-uf not = spaces
              if at01-uf not = sele-uf
                 go to lab-imp-01
              end-if
           end-if.
           if  at01-atv (01) not = 001 and 
               at01-atv (02) not = 001 and
               at01-atv (03) not = 001 and 
               at01-atv (04) not = 001 and
               at01-atv (05) not = 001
               go to lab-imp-01
           end-if.

           move at01-codigo to at02-codigo.
           move 0 to erro.
           perform rot-le-at02.
           if erro = 0
              go to lab-imp-01
           end-if.
           perform rot-move.
           add 1 to tot-impressos.
           perform dsp-tot-impressos.
           if sequencia = 3
              write reg-imp from spaces after 0 line
              write reg-imp from cab-02 after 1 line
              write reg-imp from cab-03 after 1 line
              write reg-imp from cab-04 after 1 line
              write reg-imp from cab-05 after 1 line
              write reg-imp from spaces after 2 line
              move 0 to sequencia
              move spaces to cab-02 cab-03 cab-04 cab-05
           end-if.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           if sequencia not = 0 and resposta not = "F"
              write reg-imp from spaces after 0 line
              write reg-imp from cab-02 after 1 line
              write reg-imp from cab-03 after 1 line
              write reg-imp from cab-04 after 1 line
              write reg-imp from cab-05 after 1 line
              write reg-imp from spaces after 2 line
           end-if.
           if (tot-lidos not = 0 or tot-impressos not = 0) and 
              resposta not = "F"
              move "Tecle <Enter>" to mensagem
              display tela-mensagem-cad
              perform rot-keypress
              display tela-limpa-cad
           end-if.
           perform lmp-uf thru lmp-tot-impressos.
           perform rot-close-at02.
           perform rot-close-imp.
           perform rot-close-at01.
           exit.
      *