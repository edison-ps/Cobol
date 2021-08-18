      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGAB09       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao de Aniverssariantes :                              *
      *                                                             *
      *  Data da ultima alteracao:    28/03/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgab09.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
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
           select arqimp assign to disk
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdab01.lib.
      *
       copy fdab04.lib.
      *
       fd arqimp
      
       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *
       working-storage section.
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
       01 ab04-status                  pic x(02) value "00".
       01 ab04-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab04.
          02 ab04-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab04-nome-arq             pic x(08) value "ARQAB04A".
          02 filler                    pic x(01) value ".".
          02 ab04-ext                  pic x(03) value "DAT".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAB09".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(10) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-empresa                 pic x(01) value "N".
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 linha                        pic 9(03) comp-5 value 0.
       01 coluna                       pic 9(04) comp-5 value 1.
       01 tamanho                      pic 9(04) comp-5 value 78.
       01 pagina                       pic 9(03) value 0.
       01 tracos-c                     pic x(78) value all "Ä".
       01 tracos-i                     pic x(80) value all "-".
       01 total                        pic 9(05) value 0.
       01 sequencia                    pic 9(01) value 0.
      *
       01 buffer2.
          02 filler                    pic 9(04) occurs 2000.
      *
       01 campos.
          02 sele-mes                  pic 9(02) value 0.
      *
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 cab-data.
          02 cab-dia                   pic 9(02) value 0.
          02 filler                    pic x(01) value "/".
          02 cab-mes                   pic 9(02) value 0.
      *
       01 cab-01-e.
          02 filler                    pic x(33) value spaces.
          02 cab-data-1                pic x(05) value spaces.
          02 filler                    pic x(38) value spaces.
          02 cab-data-2                pic x(05) value spaces.
          02 filler                    pic x(38) value spaces.
          02 cab-data-3                pic x(05) value spaces.
      *
       01 cab-02-e.
          02 cab-razao-1               pic x(40) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-razao-2               pic x(40) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-razao-3               pic x(40) value spaces.
      *
       01 cab-03-e.
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
       01 cab-04-e.
          02 cab-endereco-1            pic x(40) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-endereco-2            pic x(40) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-endereco-3            pic x(40) value spaces.
      *
       01 cab-05-e.
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
          02 line 13 column 37 foreground-color 06 background-color 01
             highlight value "Etiquetas".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Mes...........:".
      *
       01 tela-08.
          02 line 16 column 05 foreground-color 05 background-color 03
             pic x(41) from spaces.
          02 line 16 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 16 column 12 foreground-color 02 background-color 03
             highlight value "C".
          02 line 16 column 13 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 16 column 26 foreground-color 02 background-color 03
             highlight value "F".
          02 line 16 column 27 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-09.
          02 line 16 column 05 foreground-color 05 background-color 03
             pic x(41) from spaces.
          02 line 16 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 16 column 12 foreground-color 02 background-color 03
             highlight value "I".
          02 line 16 column 13 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-11.
          02 line 16 column 05 foreground-color 05 background-color 03
             highlight pic x(41) from spaces.
          02 line 16 column 06 foreground-color 05 background-color 03
             value "(".
          02 line 16 column 07 foreground-color 02 background-color 03
             highlight value "I".
          02 line 16 column 08 foreground-color 05 background-color 03
             value ")mprimir   (".
          02 line 16 column 20 foreground-color 02 background-color 03
             highlight value "P".
          02 line 16 column 21 foreground-color 05 background-color 03
             value ")osicionar   (".
          02 line 16 column 35 foreground-color 02 background-color 03
             highlight value "E".
          02 line 16 column 36 foreground-color 05 background-color 03
             value ")ncerrar".
      *
       01 tela-mensagem-cad.
          02 line 16 column 05 foreground-color 07 background-color 01
             highlight pic x(41) from mensagem.
      *
       01 tela-erro-cad.
          02 line 16 column 05 beep reverse-video pic x(41) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 16 column 05 foreground-color 01 background-color 01
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
           move 16 to box-lin-f.
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
       rot-le-anterior.
           move 0 to erro.
           read arqab04 previous at end move 1 to erro.
           if ab04-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqab04 next at end move 1 to erro.
           if ab04-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ab01.
           move 0 to erro.
           if ab01-stat = "F"
              open i-o arqab01
              if ab01-status not = "00"
                 move 
                 " Erro de abertura no ARQAB01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab01-stat
               end-if
           end-if.
      *
       rot-close-ab01.
           if ab01-stat = "A"
              close arqab01
              move "F" to ab01-stat
           end-if.
      *
       err-leitura-ab01.
           move " Erro de leitura - ARQAB01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-ab01.
           move 0 to erro.
           read arqab01 invalid key move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab01
           end-if.
      *
       rot-open-ab04.
           move 0 to erro.
           if ab04-stat = "F"
              open i-o arqab04
              if ab04-status not = "00"
                 move 
                 " Erro de abertura no ARQAB04A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab04-stat
               end-if
           end-if.
      *
       rot-close-ab04.
           if ab04-stat = "A"
              close arqab04
              move "F" to ab04-stat
           end-if.
      *
       err-leitura-ab04.
           move " Erro de leitura - ARQAB04A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-ab04.
           move 0 to erro.
           read arqab04 invalid key move 1 to erro.
           if ab04-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab04
           end-if.
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
       rot-move-e.
           add 1 to sequencia.
           if ab04-data-nasc not = 0
              move dia-euro to cab-dia
              move mes-euro to cab-mes
           else
              move 0 to cab-dia cab-mes
           end-if.
           evaluate true
                    when sequencia = 1
                         move cab-data to cab-data-1
                         move ab01-razao-social-a to cab-razao-1
                         move "A/C" to cab-ac-1
                         move ab04-nome-a to cab-titular-1
                         move ab01-endereco to cab-endereco-1
                         move ab01-cep to cab-cep-1
                         move ab01-cidade to cab-cidade-1
                         move ab01-uf to cab-uf-1
                    when sequencia = 2
                         move cab-data to cab-data-2
                         move ab01-razao-social-a to cab-razao-2
                         move "A/C" to cab-ac-2
                         move ab04-nome-a to cab-titular-2
                         move ab01-endereco to cab-endereco-2
                         move ab01-cep to cab-cep-2
                         move ab01-cidade to cab-cidade-2
                         move ab01-uf to cab-uf-2
                    when sequencia = 3
                         move cab-data to cab-data-3
                         move ab01-razao-social-a to cab-razao-3
                         move "A/C" to cab-ac-3
                         move ab04-nome-a to cab-titular-3
                         move ab01-endereco to cab-endereco-3
                         move ab01-cep to cab-cep-3
                         move ab01-cidade to cab-cidade-3
                         move ab01-uf to cab-uf-3
           end-evaluate.
      *
       rot-posicionar.
           move "99/99" to cab-data-1 cab-data-2 cab-data-3.
           move all "*" to cab-razao-1 cab-razao-2 cab-razao-3.
           move "A/C" to cab-ac-1 cab-ac-2 cab-ac-3.
           move all "*" to cab-titular-1 cab-titular-2 cab-titular-3.
           move all "*" to cab-endereco-1 cab-endereco-2 cab-endereco-3.
           move 99999999 to cab-cep-1 cab-cep-2 cab-cep-3.
           move all "*" to cab-cidade-1 cab-cidade-2 cab-cidade-3.
           move all "*" to cab-uf-1 cab-uf-2 cab-uf-3.
           write reg-imp from cab-01-e after 0 line.
           write reg-imp from cab-02-e after 1 line.
           write reg-imp from cab-03-e after 1 line.
           write reg-imp from cab-04-e after 1 line.
           write reg-imp from cab-05-e after 1 line.
           write reg-imp from spaces after 2 line.
      *
       rot-le-associado.
          move low-value to ab01-chave.
          move ab04-codigo to ab01-codigo.
          move ab04-condicao to ab01-condicao.
          perform rot-le-ab01.
          if erro not = 0
             move spaces to ab01-razao-social-a
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
           accept resposta at 1540 with auto foreground-color 01
                                             background-color 01.
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
       acc-mes.
           accept sele-mes at 1422 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-mes.
           display sele-mes at 1422 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-mes.
           display limpa at 1422 with foreground-color 15 
                   background-color 01.
      *
       sec-selecao section.
      *
       lab-sele-01.
           move 0 to sele-mes.
           perform lmp-mes.
           perform acc-mes.
           if escape-key = 1
              go to lab-sele-fim
           end-if.
           if sele-mes > 12 
              go to lab-sele-01
           end-if.
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
              perform lmp-mes
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-02
              end-if
           end-if.
           display tela-limpa-cad.
           perform sec-impressao-e.
           perform lmp-mes.
           display tela-limpa-cad.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform lmp-mes.
           exit.
      *
       sec-impressao-e section.
      *
       lab-imp-e-00.
           perform rot-open-ab01.
           if erro not = 0
              go to lab-imp-e-fim
           end-if.
           perform rot-open-ab04.
           if erro not = 0
              go to lab-imp-e-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-e-fim
           end-if.
           display tela-11.
           move zeros to campo-kbd.
           perform until kbd2 = 27 or 73 or 105 or 69 or 101
                   perform rot-keypress
                   if kbd2 = 80 or 112
                      perform rot-posicionar
                   end-if
           end-perform.
           if kbd2 = 27 or 69 or 101
              go to lab-imp-e-fim
           end-if.
           move 0 to sequencia.
           move low-values to ab04-chave.
           start arqab04 key is not less ab04-chave.
           display tela-09.
      *
       lab-imp-e-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-e-fim
           end-if.
           if ab04-chave = high-values
              go to lab-imp-e-01
           end-if.
           if ab04-data-nasc not = 0
              move ab04-data-nasc to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
           else 
              move 0 to mes-euro
           end-if.
           if sele-mes not = 0
              if mes-euro not = sele-mes
                 go to lab-imp-e-01
              end-if
           end-if.
           perform rot-le-associado.
           if ab01-situacao not = 1
              go to lab-imp-e-01
           end-if.
           perform rot-interrompe-impressora.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-e-fim
           end-if.
           perform rot-move-e.
           if sequencia = 3
              write reg-imp from cab-01-e after 0 line
              write reg-imp from cab-02-e after 1 line
              write reg-imp from cab-03-e after 1 line
              write reg-imp from cab-04-e after 1 line
              write reg-imp from cab-05-e after 1 line
              write reg-imp from spaces after 2 line
              move 0 to sequencia
              move spaces to cab-01-e cab-02-e cab-03-e 
                             cab-04-e cab-05-e
           end-if.
           go to lab-imp-e-01.
      * 
       lab-imp-e-fim.
           if sequencia not = 0 and resposta not = "F"
              write reg-imp from cab-01-e after 0 line
              write reg-imp from cab-02-e after 1 line
              write reg-imp from cab-03-e after 1 line
              write reg-imp from cab-04-e after 1 line
              write reg-imp from cab-05-e after 1 line
              write reg-imp from spaces after 2 line
           end-if.
           perform rot-close-imp.
           perform rot-close-ab04.
           perform rot-close-ab01.
           exit.
      *