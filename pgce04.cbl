      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGCE04       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Atualizacao de estoque :                                   *
      *                                                             *
      *  Data da ultima alteracao:    07/04/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgce04.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqce01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ce01-chave
                  alternate record key is ce01-chave-1 with duplicates
                  file status is ce01-status.
      *
           select arqce02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is automatic
                  with lock on record
                  record key is ce02-chave
                  alternate record key is ce02-chave-1 with duplicates
                  file status is ce02-status.
      *
           select arqce03 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is automatic
                  with lock on record
                  record key is ce03-chave
                  alternate record key is ce03-chave-1 with duplicates
                  alternate record key is ce03-chave-2 with duplicates
                  file status is ce03-status.
      *
       data division.
       file section.
      *    
       copy fdce01.lib.
      *
       copy fdce02.lib.
      *
       copy fdce03.lib.
      *    
       working-storage section.
      *
       01 ce01-status                  pic x(02) value "00".
       01 ce01-stat                    pic x(01) value "F".
      *
       01 nome-arq-ce01.
          02 ce01-dir                  pic x(03) value "CE2".
          02 filler                    pic x(01) value "\".
          02 ce01-nome                 pic x(08) value "ARQCE01A".
          02 filler                    pic x(01) value ".".
          02 ce01-ext                  pic x(03) value "DAT".
      *
       01 ce02-status                  pic x(02) value "00".
       01 ce02-stat                    pic x(01) value "F".
      *
       01 nome-arq-ce02.
          02 ce02-dir                  pic x(03) value "CE2".
          02 filler                    pic x(01) value "\".
          02 ce02-nome                 pic x(08) value "ARQCE02A".
          02 filler                    pic x(01) value ".".
          02 ce02-ext                  pic x(03) value "DAT".
      *
       01 ce03-status                  pic x(02) value "00".
       01 ce03-stat                    pic x(01) value "F".
      *
       01 nome-arq-ce03.
          02 ce03-dir                  pic x(03) value "CE2".
          02 filler                    pic x(01) value "\".
          02 ce03-nome                 pic x(08) value "ARQCE03A".
          02 filler                    pic x(01) value ".".
          02 ce03-ext                  pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGCE04".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa-05                     pic x(05) value spaces.
       01 limpa-10                     pic x(10) value spaces.
       01 limpa                        pic x(50) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 sele-data-i               pic 9(06) value 0.
          02 sele-data-i-disp          pic x(08) value "Inicial".
          02 sele-data-f               pic 9(06) value 99999.
          02 sele-data-f-disp          pic x(08) value "Final".
          02 sele-grupo                pic 9(05) value 0.
          02 sele-grupo-disp           pic x(05) value "Todos".
          02 sele-dgrupo               pic x(40) value spaces.
          02 sele-produto              pic 9(05) value 0.
          02 sele-produto-disp         pic x(05) value "Todos".
          02 sele-dproduto             pic x(40) value spaces.
          02 sele-atualizacao          pic x(01) value spaces.
          02 sele-data-atu             pic 9(05) value 0.
          02 produto-ant               pic 9(10) value 0.
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
          02 line 13 column 62 foreground-color 07 background-color 04
             highlight value "Atualizacao".
          02 line 14 column 06 foreground-color 06 background-color 04
             value "Data Inicial..:".
          02 line 15 column 06 foreground-color 06 background-color 04
             value "Data Final....:".
          02 line 16 column 06 foreground-color 06 background-color 04
             value "Grupo.........:".
          02 line 17 column 06 foreground-color 06 background-color 04
             value "Produto.......:".
      *
       01 tela-mensagem-cad.
          02 line 19 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 19 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 19 column 05 foreground-color 04 background-color 04
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
           move 11 to box-lin.
           move 72 to box-col-f.
           move 19 to box-lin-f.
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
       rot-rewrite-ce03.
           move 0 to erro.
           rewrite reg-ce03 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQCE03A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
           end-rewrite.
      *
       rot-rewrite-ce02.
           move 0 to erro.
           rewrite reg-ce02 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQCE02A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
           end-rewrite.
      *
       rot-pesq-controle.
           move high-values to ce03-chave-controle.
           perform rot-le-ce03-lock.
           if erro not = 0
              move " Erro no registro de controle - ARQCE03A.DAT - Tecle
      -       " <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
           else
              move ce03-atualizacao to sele-atualizacao
           end-if.
      *
       rot-le-ce03-lock.
           move 0 to erro.
           read arqce03 invalid key move 1 to erro.
           if ce03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ce03-lock
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqce03 next at end move 1 to erro.
           if ce03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ce03.
           move 0 to erro.
           if ce03-stat = "F"
              open i-o arqce03
              if ce03-status not = "00"
                 move 
                 " Erro de abertura no ARQCE03A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ce03-stat
               end-if
           end-if.
      *
       rot-close-ce03.
           if ce03-stat = "A"
              close arqce03
              move "F" to ce03-stat
           end-if.
      *
       rot-erro-leitura-ce03.
           move " Erro de leitura - ARQCE03A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-ce02.
           move 0 to erro.
           read arqce02 invalid key move 1 to erro.
           if ce02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ce02.
      *
       rot-open-ce02.
           move 0 to erro.
           if ce02-stat = "F"
              open i-o arqce02
              if ce02-status not = "00"
                 move 
                 " Erro de abertura no ARQCE02A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ce02-stat
               end-if
           end-if.
      *
       rot-close-ce02.
           if ce02-stat = "A"
              close arqce02
              move "F" to ce02-stat
           end-if.
      *
       rot-erro-leitura-ce02.
           move " Erro de leitura - ARQCE02A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-open-ce01.
           move 0 to erro.
           if ce01-stat = "F"
              open i-o arqce01
              if ce01-status not = "00"
                 move 
                 " Erro de abertura no ARQCE01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ce01-stat
               end-if
           end-if.
      *
       rot-close-ce01.
           if ce01-stat = "A"
              close arqce01
              move "F" to ce01-stat
           end-if.
      *
       rot-erro-leitura-ce01.
           move " Erro de leitura - ARQCE01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-ce01.
           move 0 to erro.
           read arqce01 invalid key move 1 to erro.
           if ce01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ce01.
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
           accept resposta at 1868 with auto foreground-color 04
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
       err-data-i.
           move " Data invalida - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-grupo-n-c.
           move " Grupo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-produto-n-c.
           move " Produto nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
      *  Sequencia para dar Accept
      *
       acc-data-i.
           accept sele-data-i at 1422 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-data-f.
           accept sele-data-f at 1522 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-grupo.
           accept sele-grupo at 1622 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-produto.
           accept sele-produto at 1722 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-data-i.
           display sele-data-i-disp at 1422 with foreground-color 15 
                   background-color 04.
      *
       dsp-data-f.
           display sele-data-f-disp at 1522 with foreground-color 15 
                   background-color 04.
      *
       dsp-grupo.
           display sele-grupo-disp at 1622 with foreground-color 15 
                   background-color 04.
           display sele-dgrupo at 1628 with foreground-color 15 
                   background-color 04.
      *
       dsp-produto.
           display sele-produto-disp at 1722 with foreground-color 15 
                   background-color 04.
           display sele-dproduto at 1728 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-data-i.
           display limpa-10 at 1422 with foreground-color 15 
                   background-color 04.
      *
       lmp-data-f.
           display limpa-10 at 1522 with foreground-color 15 
                   background-color 04.
      *
       lmp-grupo.
           display limpa at 1622 with foreground-color 15 
                   background-color 04.
      *
       lmp-produto.
           display limpa at 1722 with foreground-color 15 
                   background-color 04.
      *
       sec-selecao section.
      *
       lab-sele-00.
           display tela-limpa-cad.
           if param-prioridade < 5
              perform display-erro-usr
              go to lab-sele-fim
           end-if.
           perform rot-open-ce01.
           if erro not = 0
              go to lab-sele-fim
           end-if.
           perform rot-open-ce02.
           if erro not = 0
              go to lab-sele-fim
           end-if.
           perform rot-open-ce03.
           if erro not = 0
              go to lab-sele-fim
           end-if.
           perform rot-pesq-controle.
           unlock arqce03 record.
           if erro not = 0
              go to lab-sele-fim
           end-if.
           if sele-atualizacao = "S"
              perform dsp-data-i thru dsp-produto
              go to lab-sele-05
           end-if.
      *
       lab-sele-01.
           if ce03-data-atu not = 0
              move ce03-data-atu to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to sele-data-i
           else
              move 0 to sele-data-i
           end-if.
           perform lmp-data-i.
           perform acc-data-i.
           if escape-key = 1
              go to lab-sele-fim
           end-if.
           if sele-data-i not = 0
              move sele-data-i to data-aux
              move dia-aux to dia-euro
              move mes-aux to mes-euro
              move ano-aux to ano-euro
              move 4 to opcao-data
              perform rot-data
              if return-code not = 0
                 perform err-data-i
                 go to lab-sele-01
              end-if
              move data-disp to sele-data-i-disp
              move dias-corr to sele-data-i
           else
              move "Inicial" to sele-data-i-disp
           end-if.
           perform dsp-data-i.
      *
       lab-sele-02.
           move 0 to sele-data-f.
           perform lmp-data-f.
           perform acc-data-f.
           if escape-key = 1
              perform lmp-data-f
              go to lab-sele-01
           end-if.
           if sele-data-f not = 0
              move sele-data-f to data-aux
              move dia-aux to dia-euro
              move mes-aux to mes-euro
              move ano-aux to ano-euro
              move 4 to opcao-data
              perform rot-data
              if return-code not = 0
                 perform err-data-i
                 go to lab-sele-02
              end-if
              move data-disp to sele-data-f-disp
              move dias-corr to sele-data-f
           else
              move "Final" to sele-data-f-disp
              move 99999 to sele-data-f
           end-if.
           perform dsp-data-f.
      *
       lab-sele-03.
           move 0 to sele-grupo.
           perform lmp-grupo.
           perform acc-grupo.
           if escape-key = 1
              perform lmp-grupo
              go to lab-sele-02
           end-if.
           if sele-grupo = 0
              move "Todos" to sele-grupo-disp sele-produto-disp
              move spaces to sele-dgrupo sele-dproduto
              perform dsp-grupo thru dsp-produto
              go to lab-sele-05
           end-if.
           move sele-grupo to ce01-grupo sele-grupo-disp.
           perform rot-le-ce01.
           if erro not = 0
              perform err-grupo-n-c
              go to lab-sele-03
           end-if.
           move ce01-descricao-a to sele-dgrupo.
           perform dsp-grupo.
      *
       lab-sele-04.
           move 0 to sele-produto.
           perform lmp-produto.
           perform acc-produto.
           if escape-key = 1
              perform lmp-produto
              go to lab-sele-03
           end-if.
           if sele-produto = 0
              move "Todos" to sele-produto-disp
              move spaces to sele-dproduto
              perform dsp-produto
              go to lab-sele-05
           end-if.
           move sele-grupo to ce02-grupo.
           move sele-produto to ce02-produto sele-produto-disp.
           perform rot-le-ce02.
           if erro not = 0
              perform err-produto-n-c
              go to lab-sele-04
           end-if.
           move ce02-descricao-a to sele-dproduto.
           perform dsp-produto.
      *
       lab-sele-05.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              if sele-grupo = 0
                 perform lmp-produto
                 go to lab-sele-03
              else
                 go to lab-sele-04
           end-if.
           if resposta = "N"
              perform lmp-data-i thru lmp-produto
              display tela-limpa-cad
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-05
              end-if
           end-if.
           perform sec-atualizacao.
           if erro = 0
              move " Atualizacao efetuada - Tecle <Enter>" to mensagem
              display tela-mensagem-cad
              perform rot-keypress
           end-if.
      *
       lab-sele-fim.
           perform rot-close-ce01.
           perform rot-close-ce02.
           perform rot-close-ce03.
           exit.
      *
       sec-atualizacao section.
      *
       lab-atu-00.
           display tela-limpa-cad.
           move low-values to ce03-chave-1.
           move sele-data-i to ce03-data-mov.
           move sele-grupo to ce03-grupo in ce03-chave-1.
           move sele-produto to ce03-produto in ce03-chave-1.
           start arqce03 key is not less ce03-chave-1
                 invalid key move 1 to erro
                             perform rot-erro-leitura-ce01
                             go to lab-atu-fim
           end-start.
           move 0 to produto-ant.
      *
       lab-atu-01.
           perform rot-le-proximo.
           if erro not = 0
              if ce03-status = "10"
                 if produto-ant not = 0
                    move param-usr to ce02-usuario
                    move param-data to ce02-data
                    perform rot-rewrite-ce02
                    if erro not = 0
                       go to lab-atu-fim
                    end-if
                    move 0 to erro
                 end-if
              end-if
              go to lab-atu-fim
           end-if.
           if ce03-chave = high-values 
              go to lab-atu-01
           end-if.
           if ce03-data-mov > sele-data-f
              go to lab-atu-fim
           end-if.
           if sele-grupo not = 0
              if ce03-grupo in ce03-chave-1 not = sele-grupo
                 go to lab-atu-01
              end-if
           end-if.
           if sele-produto not = 0
              if ce03-produto in ce03-chave-1 not = 0
                 go to lab-atu-01
              end-if
           end-if.       
           if sele-atualizacao not = "S"
              if ce03-flag-atu not = "N"
                 go to lab-atu-01
              end-if
           end-if.
           if ce03-chave-2 not = produto-ant
              if produto-ant not = 0
                 move param-usr to ce02-usuario
                 move param-data to ce02-data
                 perform rot-rewrite-ce02
                 if erro not = 0
                    go to lab-atu-fim
                 end-if
              end-if
              move ce03-chave-2 to produto-ant ce02-chave
              perform rot-le-ce02
              if erro not = 0
                 perform rot-erro-leitura-ce02
              end-if
           end-if.
      *
       lab-atu-02.
           evaluate true
                    when ce03-movimento = 1
                         move ce03-quantidade to ce02-estoque-real
                                                 ce02-estoque-ofic
                    when ce03-movimento = 2
                         add ce03-quantidade to ce02-estoque-real
                         if ce03-nf not = spaces
                            add ce03-quantidade to ce02-estoque-ofic
                         end-if
                    when ce03-movimento = 3
                         subtract ce03-quantidade from ce02-estoque-real
                         if ce03-nf not = spaces
                            subtract ce03-quantidade from 
                                     ce02-estoque-ofic
                         end-if
                    when ce03-movimento = 4
                         subtract ce03-quantidade from ce02-estoque-real
                         if ce03-nf not = spaces
                            subtract ce03-quantidade from 
                                     ce02-estoque-ofic
                         end-if
                    when ce03-movimento = 5
                         subtract ce03-quantidade from ce02-estoque-real
                         if ce03-nf not = spaces
                            subtract ce03-quantidade from 
                                     ce02-estoque-ofic
                         end-if
           end-evaluate.
           move "S" to ce03-flag-atu.
           perform rot-rewrite-ce03.
           if erro not = 0
              go to lab-atu-fim
           end-if.
           move ce03-data-mov to sele-data-atu
           go to lab-atu-01.
      *
       lab-atu-fim.
           if erro = 0
              perform rot-pesq-controle
              if erro = 0 and sele-data-i = 0 and 
                 sele-data-f = 99999 and sele-grupo = 0 
                 and sele-produto = 0
                 move "N" to ce03-atualizacao
                 move sele-data-atu to ce03-data-atu
                 perform rot-rewrite-ce03
              end-if
           end-if.
           exit.
