      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGAB02       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao do cadastro Balcao :                            *
      *                                                             *
      *  Data da ultima alteracao:    16/11/93     v1.00            *
      *                               28/02/94     v1.01            *
      *                               11/02/96     v1.02            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgab02.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
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
       data division.
       file section.
      *    
       copy fdab02.lib.
      *    
       copy fdab01.lib.
      *
       working-storage section.
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
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAB02".
          02 cb-versao                 pic x(06) value "v1.02 ".
      *
       01 limpa                        pic x(45) value spaces.
       01 limpa-aux                    pic x(05) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 codigo                    pic 9(05) value 0.
          02 razao-social              pic x(40) value spaces.
          02 razao-social-a            pic x(40) value spaces.
          02 rateio                    pic x(01) value spaces.
          02 faixa                     pic x(02) value spaces.
      * 
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 cab-usr.
          02 filler                    pic x(10) value "Usuario.:".
          02 cab-usuario               pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Data....:".
          02 cab-data                  pic x(08) value spaces.
      *
       01 campo-rotina-cod.
          02 rotina-col-cod            pic 9(02) value 0.
          02 rotina-lin-cod            pic 9(02) value 0.
          02 rotina-borda-cod          pic x(01) value spaces.
          02 rotina-fundo-cod          pic x(01) value spaces.
          02 rotina-sombra-cod         pic x(01) value spaces.
          02 rotina-codigo-cod         pic 9(05) value 0.
          02 rotina-condicao-cod       pic x(01) value spaces.
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
       01 campo-rotina-ab02.
          02 rotina-ab02-codigo        pic 9(05).
          02 rotina-ab02-nivel         pic 9(01).
      *
       screen section.
      *
       01 tela-01.
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Codigo........:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Razao Social..:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "Rateio........:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "Faixa.........:".
      *
       01 tela-02.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 20 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 20 column 16 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 20 column 18 foreground-color 05 background-color 03
             value "-Consultas".
          02 line 20 column 31 foreground-color 02 background-color 03
             highlight value "F3".
          02 line 20 column 33 foreground-color 05 background-color 03
             value "-Associados".
      *
       01 tela-03.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 20 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 20 column 16 foreground-color 02 background-color 03
             highlight value "C".
          02 line 20 column 17 foreground-color 05 background-color 03
             value "odigo".
          02 line 20 column 27 foreground-color 02 background-color 03
             highlight value "R".
          02 line 20 column 28 foreground-color 05 background-color 03
             value "azao Social".
      *
       01 tela-04.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 20 column 08 foreground-color 05 background-color 03
             value "-Alt".
          02 line 20 column 15 foreground-color 02 background-color 03 
             highlight value "F3".
          02 line 20 column 17 foreground-color 05 background-color 03
             value "-Exc".
          02 line 20 column 25 foreground-color 02 background-color 03
             highlight value "Home".
          02 line 20 column 29 foreground-color 05 background-color 03
             value "-Inic".
          02 line 20 column 37 foreground-color 02 background-color 03
             highlight value "End".
          02 line 20 column 40 foreground-color 05 background-color 03
             value "-Fim".
          02 line 20 column 47 foreground-color 02 background-color 03
             highlight value "PgDown".
          02 line 20 column 53 foreground-color 05 background-color 03
             value "-Prox".
          02 line 20 column 60 foreground-color 02 background-color 03
             highlight value "PgUp".
          02 line 20 column 64 foreground-color 05 background-color 03
             value "-Ant".
      *
       01 tela-05.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 02 background-color 03 
             highlight value "S".
          02 line 20 column 07 foreground-color 05 background-color 03
             value " - Sim".
          02 line 20 column 18 foreground-color 02 background-color 03 
             highlight value "N".
          02 line 20 column 19 foreground-color 05 background-color 03
             value " - Nao".
      *
       01 tela-06.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-09.
          02 line 14 column 65 foreground-color 06 background-color 01
             highlight value "Inclusao".
      *
       01 tela-10.
          02 line 14 column 65 foreground-color 06 background-color 01
             highlight value "Consulta".
      *
       01 tela-11.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 20 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 20 column 16 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 20 column 18 foreground-color 05 background-color 03
             value "-Consultas".
      *
       01 tela-mensagem-cad.
          02 line 20 column 05 foreground-color 07 background-color 01
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 20 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 20 column 05 foreground-color 01 background-color 01
             pic x(68) from spaces.
      *
       copy scrgen.lib.
      *
       procedure division using param-menu campo-rotina-ab02.
      *
       lab-00.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-save-buffer.
           display tela-cabec.
           move 03 to box-col.
           move 12 to box-lin.
           move 72 to box-col-f.
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
           perform sec-inclusao.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-rest-buffer.
      *
       lab-fim.
           move ab02-codigo to rotina-ab02-codigo.
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
       rot-move-ab02.
           move codigo to ab02-codigo.
           move razao-social to ab02-chave-1.
           move razao-social-a to ab02-razao-social-a.
           move rateio to ab02-rateio.
           move faixa to ab02-faixa.
           move param-usr to ab02-usuario.
           move param-data to ab02-data.
      *
       rot-move-campos.
           move ab02-codigo to codigo.
           move ab02-razao-social-a to razao-social.
           move ab02-rateio to rateio.
           move ab02-faixa to faixa.
           move ab02-data to dias-corr.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move ab02-usuario to cab-usuario.
      *
       rot-le-ab02.
           move 0 to erro.
           read arqab02 invalid key move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab02.
      *
       rot-le-ab02-1.
           move 0 to erro.
           read arqab02 key ab02-chave-1 invalid key move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab02-1.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqab02 key is equal ab02-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-ab02
           end-start.
      *
       rot-le-ab02-lock.
           move 0 to erro.
           read arqab02 next. 
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ab02-lock
           end-if.
           read arqab02 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqab02 previous at end move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqab02 next at end move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ab02.
           move 0 to erro.
           if ab02-stat = "F"
              open i-o arqab02
              if ab02-status not = "00"
                 move 
                 " Erro de abertura no ARQAB02A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab02-stat
               end-if
           end-if.
      *
       rot-close-ab02.
           if ab02-stat = "A"
              close arqab02
              move "F" to ab02-stat
           end-if.
      *
       rot-erro-leitura-ab02.
           move " Erro de leitura - ARQAB02A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
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
       rot-erro-leitura-ab01.
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
       rot-inic-arquivo.
           perform lmp-codigo thru lmp-faixa.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-codigo thru lmp-faixa.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-display.
           perform rot-move-campos.
           perform dsp-codigo thru dsp-faixa.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-pesq-associado.
           perform rot-close-ab01.
           move 08 to rotina-col-cod.
           move 10 to rotina-lin-cod.
           move "3" to rotina-borda-cod.
           move spaces to rotina-fundo-cod.
           move "S" to rotina-sombra-cod.
           move "A" to rotina-condicao-cod.
           call "rotab01" using param-menu campo-rotina-cod.
           cancel "rotab01".
           perform rot-open-ab01.
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
           accept resposta at 1868 with auto foreground-color 01
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
       rot-n-cad.
           move " Codigo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-j-cad.
           move " Codigo ja cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
      *  Sequencia para dar Accept
      *
       acc-codigo.
           accept codigo at 1522 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-razao-social.
           accept razao-social at 1622 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-rateio.
           accept rateio at 1722 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-faixa.
           accept faixa at 1822 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo.
           display codigo at 1522 with foreground-color 15 
                   background-color 01.
      *
       dsp-razao-social.
           display razao-social at 1622 with foreground-color 15 
                   background-color 01.
      *
       dsp-rateio.
           display rateio at 1722 with foreground-color 15 
                   background-color 01.
      *
       dsp-faixa.
           display faixa at 1822 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-codigo.
           display limpa at 1522 with foreground-color 15 
                   background-color 01.
      *
       lmp-razao-social.
           display limpa at 1622 with foreground-color 15 
                   background-color 01.
      *
       lmp-rateio.
           display limpa at 1722 with foreground-color 15 
                   background-color 01.
      *
       lmp-faixa.
           display limpa at 1822 with foreground-color 15 
                   background-color 01.
      *
       sec-inclusao section.
      *
       lab-inc-00.
           display tela-limpa-cad.
           perform rot-open-ab02.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-ab01.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           display tela-09.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
           move 0 to rotina-codigo-cod.
      *
       lab-inc-01.
           display tela-09.
           if rotina-ab02-nivel < 1
              display tela-02
           else
              display tela-11
           end-if.
           move rotina-codigo-cod to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform lmp-codigo
              perform sec-consulta
              display tela-09
              go to lab-inc-01
           end-if.
           if rotina-ab02-nivel < 1
              if escape-key = 4
                 perform rot-pesq-associado
                 go to lab-inc-01
              end-if
           end-if.
           if codigo = 0
              go to lab-inc-01
           end-if.
           move codigo to ab01-codigo ab02-codigo.
           move "A" to ab01-condicao.
           perform rot-le-ab01.
           if erro not = 0
              perform rot-n-cad
              go to lab-inc-01
           end-if.
           perform rot-le-ab02.
           if erro = 0
              perform rot-j-cad
              go to lab-inc-01
           end-if.
           move ab01-razao-social-a to razao-social.
           display tela-limpa-cad.
      *
       lab-inc-02.
           perform acc-razao-social.
           if escape-key = 1
              perform lmp-razao-social
              move 0 to rotina-codigo-cod
              go to lab-inc-01
           end-if.
           move razao-social to txt razao-social-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-02
           end-if.
           move txt to razao-social.
      *
       lab-inc-03.
           display tela-05.
           move spaces to rateio.
           perform lmp-rateio.
           perform acc-rateio.
           if escape-key = 1
              perform lmp-rateio
              display tela-limpa-cad
              move razao-social-a to razao-social
              go to lab-inc-02
           end-if.
           move rateio to txt.
           perform rot-texto.
           if txt not = "N" and "S"
              go to lab-inc-03
           end-if.
           move txt to  rateio.
           perform dsp-rateio.
           display tela-limpa-cad.
      *
       lab-inc-03-01.
           move spaces to faixa.
           perform lmp-faixa.
           perform acc-faixa.
           if escape-key = 1
              perform lmp-faixa
              go to lab-inc-03
           end-if.
           move faixa to txt.
           perform rot-texto.
           if txt not = "A1" and "A2" and "B1" and "B2" and "C" and "D"
                         and "E" and "F" and "G" and "H" and spaces
              go to lab-inc-03-01
           end-if.
           move txt to faixa.
           perform dsp-faixa.

      *
       lab-inc-04.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-03-01
           end-if.
           if resposta = "N"
              perform lmp-codigo thru lmp-faixa
              move 0 to rotina-codigo-cod
              display tela-02
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-04
              end-if
           end-if.
      *
       lab-inc-07.
           perform rot-move-ab02.
           write reg-ab02 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQAB02A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           display tela-06.
           perform rot-keypress.
           perform lmp-codigo thru lmp-faixa.
           move 0 to rotina-codigo-cod
           display tela-02.
           go to lab-inc-01.
      *
       lab-inc-fim.
           perform rot-close-ab01.
           perform rot-close-ab02.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           display tela-limpa-cad.
           display tela-10.
      *
       lab-cns-01.
           display tela-03.
           move 0 to kbd2.
           perform until kbd2 = 27
                   perform rot-keypress
                   evaluate true
                            when kbd2 = 67 or 99
                                 display tela-limpa-cad
                                 perform sec-consulta-codigo
                                 display tela-03
                            when kbd2 = 82 or 114
                                 display tela-limpa-cad
                                 perform sec-consulta-razao-social
                                 display tela-03
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-fim.
           display tela-limpa.
           exit.
      *
       sec-consulta-codigo section.
      *
       lab-cns-codigo-00.
           move 0 to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              perform lmp-codigo
              go to lab-cns-codigo-fim
           end-if.
           display tela-04.
           move low-values to ab02-chave.
           move codigo to ab02-codigo.
      *
       lab-cns-codigo-00-a.
           start arqab02 key is not less ab02-chave.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or ab02-codigo = codigo
              perform rot-inic-arquivo
              start arqab02 key is not less ab02-chave
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if ab02-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqab02 key is less ab02-chave.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ab02
              go to lab-cns-codigo-fim
           end-if.
           if ab02-chave = high-values
              perform rot-fim-arquivo
              start arqab02 key is not less ab02-chave
              move 0 to codigo
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
      *
       lab-cns-codigo-04.
           perform rot-display.
      *
       lab-cns-codigo-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-codigo-03
                    when kbd-aux = 73
                         go to lab-cns-codigo-01
                    when kbd-aux = 71
                         move low-values to ab02-chave
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to ab02-chave
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-codigo thru lmp-faixa.
           display tela-limpa-cad.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-faixa.
           exit.
      *
       sec-consulta-razao-social section.
      *
       lab-cns-razao-social-00.
           move spaces to razao-social.
           perform lmp-razao-social.
           perform acc-razao-social.
           if escape-key = 1
              perform lmp-razao-social
              go to lab-cns-razao-social-fim
           end-if.
           display tela-04.
           move razao-social to txt.
           perform rot-texto.
           move low-values to ab02-chave-1.
           move txt to ab02-razao-social in ab02-chave-1.
      *
       lab-cns-razao-social-00-a.
           start arqab02 key is not less ab02-chave-1.
           go to lab-cns-razao-social-03.
      *
       lab-cns-razao-social-01.
           perform rot-le-anterior.
           if erro not = 0 or ab02-codigo = codigo
              perform rot-inic-arquivo
              start arqab02 key is not less ab02-chave-1
              move 1 to erro
              go to lab-cns-razao-social-05
           end-if.
           if ab02-chave = high-values
              go to lab-cns-razao-social-01
           end-if.
           go to lab-cns-razao-social-04.
      *
       lab-cns-razao-social-02.
           start arqab02 key is less ab02-chave-1.
      *
       lab-cns-razao-social-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ab02
              go to lab-cns-razao-social-fim
           end-if.
           if ab02-chave = high-values
              perform rot-fim-arquivo
              start arqab02 key is not less ab02-chave-1
              move 0 to codigo
              move 1 to erro
              go to lab-cns-razao-social-05
           end-if.
      *
       lab-cns-razao-social-04.
           perform rot-display.
      *
       lab-cns-razao-social-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-razao-social-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-razao-social-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-razao-social-03
                    when kbd-aux = 73
                         go to lab-cns-razao-social-01
                    when kbd-aux = 71
                         move low-values to ab02-chave-1
                         go to lab-cns-razao-social-00-a
                    when kbd-aux = 79
                         move high-values to ab02-chave-1
                         go to lab-cns-razao-social-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-razao-social-05
           end-if.
           perform lmp-codigo thru lmp-faixa.
           display tela-limpa-cad.
           go to lab-cns-razao-social-00.
      *
       lab-cns-razao-social-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-faixa.
           exit.
      *
       sec-exclusao section.
       lab-exc-00-0.
           display tela-limpa-cad.
           if param-prioridade < 7
              perform display-erro-usr
              go to lab-exc-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-exc-fim
           end-if.
      *
       lab-exc-00.
           perform rot-le-ab02-lock.
           perform rot-display
           move "Excluir (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-exc-01.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-exc-fim
           end-if.
           if resposta = "N"
              go to lab-exc-fim
           else
              if resposta not = "S"
                 go to lab-exc-01
              end-if
           end-if.
           delete arqab02 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQAB02A.DAT - Tecle <Enter>
      -           " " to mensagem
                  display tela-erro
                  perform rot-keypress
                  display tela-limpa
                  go to lab-exc-fim
           end-delete.
           move "Registro excluido - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       lab-exc-fim.
           unlock arqab02 record.
           display tela-04.
           exit.
      *
       sec-alteracao section.
       lab-alt-00-0.
           display tela-limpa-cad.
           if param-prioridade < 5
              perform display-erro-usr
              go to lab-alt-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-alt-fim
           end-if.
      *
       lab-alt-00.
           perform rot-le-ab02-lock.
           perform rot-display.
      *
       lab-alt-01.
           perform acc-razao-social.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           move razao-social to txt razao-social-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-01
           end-if.
           move txt to razao-social ab02-chave-1.
           move 0 to erro.
           start arqab02 key is equal ab02-chave-1 invalid key 
                 display tela-limpa-cad
                 go to lab-alt-02
           end-start.
           perform rot-le-proximo.
           if ab02-codigo not = codigo
              perform rot-j-cad
              go to lab-alt-01
           end-if.
           display tela-limpa-cad.
      *
       lab-alt-02.
           display tela-05.
           perform acc-rateio.
           if escape-key = 1
              perform lmp-rateio
              display tela-limpa-cad
              move razao-social-a to razao-social
              go to lab-alt-01
           end-if.
           move rateio to txt.
           perform rot-texto.
           if txt not = "N" and "S"
              go to lab-alt-02
           end-if.
           move txt to  rateio.
           perform dsp-rateio.
           display tela-limpa-cad.
      *
       lab-alt-02-01.
           perform lmp-faixa.
           perform acc-faixa.
           if escape-key = 1
              perform lmp-faixa
              go to lab-alt-02
           end-if.
           move faixa to txt.
           perform rot-texto.
           if txt not = "A1" and "A2" and "B1" and "B2" and "C" and "D"
                         and "E" and "F" and "G" and "H" and spaces
              go to lab-alt-02-01
           end-if.
           move txt to faixa.
           perform dsp-faixa.
      *
       lab-alt-03.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-02-01
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-03
              end-if
           end-if.
           perform rot-move-ab02.
           rewrite reg-ab02 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQAB02A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-alt-fim
           end-rewrite.
           move "Registro alterado - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       lab-alt-fim.
           unlock arqab02 record.
           display tela-04.
           exit.