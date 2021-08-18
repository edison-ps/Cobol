      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGFT05       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Documento de Cobranca:                                     *
      *                                                             *
      *  Data da ultima alteracao:    07/10/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgft05.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqrc01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is automatic
                  with lock on multiple records
                  record key is rc01-chave
                  alternate record key is rc01-chave-1 with duplicates
                  alternate record key is rc01-chave-2 with duplicates
                  alternate record key is rc01-chave-3 with duplicates
                  file status is rc01-status.
      *
       data division.
       file section.
      *    
       copy fdrc01.lib.
      *    
       working-storage section.
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
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGFT05".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(48) value spaces.
       01 limpa-06                     pic x(06) value spaces.
       01 limpa-aux                    pic x(05) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 sele-doc-i                pic 9(06) value 0.
          02 sele-doc-f                pic 9(06) value 0.
          02 sele-doc-cob-i            pic 9(06) value 0.
          02 sele-doc-cob-f            pic 9(06) value 0.
      * 
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       copy workgen.lib.
       copy wstab01.lib.
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
          02 line 09 column 54 foreground-color 06 background-color 01
             highlight value "Doctos. de Cobranca".
          02 line 10 column 06 foreground-color 06 background-color 01
             highlight value "Documento......:        a".
          02 line 11 column 06 foreground-color 06 background-color 01
             highlight value "Docto. Cobranca:        a".
          02 line 13 column 06 foreground-color 06 background-color 01
             highlight value "Documento......:".
          02 line 13 column 30 foreground-color 06 background-color 01
             highlight value "Docto. Cobranca:".
      *
       01 tela-mensagem-cad.
          02 line 15 column 05 foreground-color 07 background-color 01
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 15 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 15 column 05 foreground-color 01 background-color 01
             pic x(68) from spaces.
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
           move 07 to box-lin.
           move 72 to box-col-f.
           move 15 to box-lin-f.
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
           perform sec-doc-cob.
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
       rot-le-rc01.
           move 0 to erro.
           read arqrc01 invalid key move 1 to erro.
           if rc01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-rc01.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqrc01 key is equal rc01-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-rc01
           end-start.
      *
       rot-le-rc01-lock.
           move 0 to erro.
           read arqrc01 next. 
           if rc01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-rc01-lock
           end-if.
           read arqrc01 with kept lock.
      *
       rot-open-rc01.
           move 0 to erro.
           if rc01-stat = "F"
              open i-o arqrc01
              if rc01-status not = "00"
                 move 
                 " Erro de abertura no ARQRC01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to rc01-stat
               end-if
           end-if.
      *
       rot-close-rc01.
           if rc01-stat = "A"
              close arqrc01
              move "F" to rc01-stat
           end-if.
      *
       rot-erro-leitura-rc01.
           move " Erro de leitura - ARQRC01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-le-rc01-3.
           move 0 to erro.
           read arqrc01 key is rc01-chave-3 invalid key move 1 to erro.
           if rc01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-rc01
           end-if.
      *
       rot-rewrite.
           rewrite reg-rc01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQRC01A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
           end-rewrite.
      *
       err-doc-maior.
           move " Docto. final nenor que inicial - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-doc-n-c.
           move " Numeros do Doctos. nao confere - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-doc-n.
           move " Documento nao cadastrado - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-doc-n-e.
           move " Documento nao encontrado - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-doc-c.
           move " Documento ja cadastrado - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
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
           accept resposta at 1368 with auto foreground-color 01
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
       acc-doc-i.
           accept sele-doc-i at 1023 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-doc-f.
           accept sele-doc-f at 1032 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-doc-cob-i.
           accept sele-doc-cob-i at 1123 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-doc-cob-f.
           accept sele-doc-cob-f at 1132 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-doc-i.
           display sele-doc-i at 1023 with foreground-color 15 
                   background-color 01.
      *
       dsp-doc-f.
           display sele-doc-f at 1032 with foreground-color 15 
                   background-color 01.
      *
       dsp-doc-cob-i.
           display sele-doc-cob-i at 1123 with foreground-color 15 
                   background-color 01.
      *
       dsp-doc-cob-f.
           display sele-doc-cob-f at 1132 with foreground-color 15
                   background-color 01.
      *
       dsp-doc-disp.
           display sele-doc-i at 1323 with foreground-color 15 
                   background-color 01.
      *
       dsp-doc-cob-disp.
           display sele-doc-cob-i at 1347 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-doc-i.
           display limpa-06 at 1023 with foreground-color 15 
                   background-color 01.
      *
       lmp-doc-f.
           display limpa-06 at 1032 with foreground-color 15 
                   background-color 01.
      *
       lmp-doc-cob-i.
           display limpa-06 at 1123 with foreground-color 15 
                   background-color 01.
      *
       lmp-doc-cob-f.
           display limpa-06 at 1132 with foreground-color 15 
                   background-color 01.
      *
       lmp-doc-disp.
           display limpa-06 at 1323 with foreground-color 15 
                   background-color 01.
      *
       lmp-doc-cob-disp.
           display limpa-06 at 1347 with foreground-color 15 
                   background-color 01.
      *
       sec-doc-cob section.
      *
       lab-doc-00.
           display tela-limpa-cad.
           if param-prioridade < 8
              perform display-erro-usr
              go to lab-doc-fim
           end-if.
           perform rot-open-rc01.
           if erro not = 0
              go to lab-doc-fim
           end-if.
      *
       lab-doc-01.
           move 0 to sele-doc-i.
           perform lmp-doc-i.
           perform acc-doc-i.
           if escape-key = 1
              go to lab-doc-fim
           end-if.
           if sele-doc-i = 0
              go to lab-doc-01
           end-if.
           move sele-doc-i to rc01-documento.
           perform rot-le-rc01.
           if erro not = 0
              perform err-doc-n
              go to lab-doc-01
           end-if.
      *
       lab-doc-02.
           move 0 to sele-doc-f.
           perform lmp-doc-f.
           perform acc-doc-f.
           if escape-key = 1
              perform lmp-doc-f
              go to lab-doc-01
           end-if.
           if sele-doc-f = 0
              go to lab-doc-02
           end-if.
           if sele-doc-f < sele-doc-i
              perform err-doc-maior
              go to lab-doc-02
           end-if.
           move sele-doc-f to rc01-documento.
           perform rot-le-rc01.
           if erro not = 0
              perform err-doc-n
              go to lab-doc-02
           end-if.
      *
       lab-doc-03.
           move 0 to sele-doc-cob-i.
           perform lmp-doc-cob-i.
           perform acc-doc-cob-i.
           if escape-key = 1
              perform lmp-doc-cob-i
              go to lab-doc-02
           end-if.
           if sele-doc-cob-i = 0
              go to lab-doc-03
           end-if.
      *
       lab-doc-04.
           move 0 to sele-doc-cob-f.
           perform lmp-doc-cob-f.
           perform acc-doc-cob-f.
           if escape-key = 1
              perform lmp-doc-cob-f
              go to lab-doc-03
           end-if.
           if sele-doc-cob-f = 0
              go to lab-doc-04
           end-if.
           if sele-doc-cob-f < sele-doc-cob-i
              perform err-doc-maior
              go to lab-doc-04
           end-if.
           if (sele-doc-cob-f - sele-doc-cob-i) not = 
              (sele-doc-f - sele-doc-i)
              perform err-doc-n-c
              go to lab-doc-04
           end-if.
      *
       lab-doc-05.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-doc-04
           end-if.
           if resposta = "N"
              perform lmp-doc-i thru lmp-doc-cob-f
              display tela-limpa-cad
              go to lab-doc-01
           else
              if resposta not = "S"
                 go to lab-doc-05
              end-if
           end-if.
           display tela-limpa-cad.
           perform sec-numeracao.
           perform lmp-doc-i thru lmp-doc-cob-disp.
           go to lab-doc-01.
      *
       lab-doc-fim.
           perform rot-close-rc01.
           exit.
      *
       sec-numeracao section.
      *
       lab-num-00.
           if sele-doc-i > sele-doc-f
              move "Tecle <Enter>" to mensagem
              display tela-mensagem-cad
              perform rot-keypress              
              display tela-limpa-cad
              go to lab-num-fim
           end-if.
           perform dsp-doc-disp thru dsp-doc-cob-disp.
           move sele-doc-cob-i to rc01-doc-cob.
           perform rot-le-rc01-3.
           if erro = 0
              perform err-doc-c
              add 1 to sele-doc-i sele-doc-cob-i
              go to lab-num-00
           end-if.
           move sele-doc-i to rc01-documento.
           perform rot-le-rc01.
           if erro not = 0
              perform err-doc-n-e
              add 1 to sele-doc-i sele-doc-cob-i
              go to lab-num-00
           end-if.
           perform rot-ponteiro.
           perform rot-le-rc01-lock.
           if rc01-doc-cob = spaces or "AUTO"
              move sele-doc-cob-i to rc01-doc-cob
              perform rot-rewrite
              if erro not = 0 
                 go to lab-num-fim
              end-if
              add 1 to sele-doc-i sele-doc-cob-i
              unlock arqrc01
              go to lab-num-00
           end-if.
      *
       lab-num-01.
           move "Documento ja numerado Confirma (S) (N) ?" to mensagem.
           display tela-erro-cad.
           perform accept-resposta-cad.
           if escape-key = 1 or resposta = "N"
              display tela-limpa-cad
              add 1 to sele-doc-i sele-doc-cob-i
              unlock arqrc01
              go to lab-num-00
           else
              if resposta not = "S"
                 go to lab-num-01
              end-if
           end-if.
           move sele-doc-cob-i to rc01-doc-cob.
           perform rot-rewrite.
           if erro not = 0 
              go to lab-num-fim
           end-if.
           add 1 to sele-doc-i sele-doc-cob-i.
           unlock arqrc01.
           go to lab-num-00.
      *
       lab-num-fim.
           exit.