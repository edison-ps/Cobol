      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGCE03       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Movimentacao do estoque :                                  *
      *                                                             *
      *  Data da ultima alteracao:    05/04/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgce03.
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
                  lock mode is manual
                  with lock on multiple records
                  record key is ce02-chave
                  alternate record key is ce02-chave-1 with duplicates
                  file status is ce02-status.
      *
           select arqce03 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ce03-chave
                  alternate record key is ce03-chave-1 with duplicates
                  alternate record key is ce03-chave-2 with duplicates
                  file status is ce03-status.
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
       data division.
       file section.
      *    
       copy fdce01.lib.
      *
       copy fdce02.lib.
      *
       copy fdce03.lib.
      *    
       copy fdcd01.lib.
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
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGCE03".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa-07                     pic x(07) value spaces.
       01 limpa-10                     pic x(10) value spaces.
       01 limpa                        pic x(50) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 codigo                    pic 9(06) value 0.
          02 data-mov                  pic 9(06) value 0.
          02 data-mov-disp             pic x(08) value spaces.
          02 movimento                 pic 9(01) value 0.
          02 grupo                     pic 9(05) value 0.
          02 dgrupo                    pic x(40) value spaces.
          02 unidade                   pic x(10) value spaces.
          02 produto                   pic 9(05) value 0.
          02 dproduto                  pic x(40) value spaces.
          02 cliente                   pic 9(05) value 0.
          02 cliente-disp              pic 9(05) value 0
             blank when zero.
          02 dcliente                  pic x(40) value spaces.
          02 nf                        pic x(10) value spaces.
          02 quantidade                pic zz.zzz value 0.
          02 quantidade-aux            pic 9(05) value 0.
          02 preco-aux                 pic z(10)9,9(02) value 0.
          02 preco                     pic 9(11)v9(02) value 0.
          02 preco-disp                pic zz.zzz.zzz.zz9,99 value 0
             blank when zero.
          02 obs                       pic x(40) value spaces.
          02 flag-atu                  pic x(02) value spaces.
          02 flag-fat                  pic x(01) value spaces.
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
          02 line 07 column 06 foreground-color 06 background-color 04
             value "Data..........:".
          02 line 08 column 06 foreground-color 06 background-color 04
             value "Movimento.....:".
          02 line 09 column 06 foreground-color 06 background-color 04
             value "Grupo.........:".
          02 line 10 column 06 foreground-color 06 background-color 04
             value "Produto.......:".
          02 line 11 column 06 foreground-color 06 background-color 04
             value "Cliente.......:".
          02 line 13 column 06 foreground-color 06 background-color 04
             value "N.F...........:".
          02 line 14 column 06 foreground-color 06 background-color 04
             value "Quantidade....:".
          02 line 15 column 06 foreground-color 06 background-color 04
             value "Preco.........:".
          02 line 16 column 06 foreground-color 06 background-color 04
             value "Observacao....:".
          02 line 17 column 06 foreground-color 06 background-color 04
             value "Atualizacao...:".
          02 line 18 column 06 foreground-color 06 background-color 04
             value "Faturamento...:".
      *
       01 tela-02.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight value "F1".
          02 line 20 column 07 foreground-color 01 background-color 02
             value "-Help".
          02 line 20 column 15 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 20 column 17 foreground-color 01 background-color 02
             value "-Consultas".
      *
       01 tela-03.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 07 background-color 
             02 highlight value "F1".
          02 line 20 column 08 foreground-color 01 background-color 02
             value "-Help".
          02 line 20 column 16 foreground-color 07 background-color 02
             highlight value "D".
          02 line 20 column 17 foreground-color 01 background-color 02
             value "ata".
          02 line 20 column 23 foreground-color 07 background-color 02
             highlight value "P".
          02 line 20 column 24 foreground-color 01 background-color 02
             value "roduto".
      *
       01 tela-04.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 07 background-color 02 
             highlight value "F2".
          02 line 20 column 08 foreground-color 01 background-color 02
             value "-Alt".
          02 line 20 column 15 foreground-color 07 background-color 02 
             highlight value "F3".
          02 line 20 column 17 foreground-color 01 background-color 02
             value "-Exc".
          02 line 20 column 25 foreground-color 07 background-color 02
             highlight value "Home".
          02 line 20 column 29 foreground-color 01 background-color 02
             value "-Inic".
          02 line 20 column 37 foreground-color 07 background-color 02
             highlight value "End".
          02 line 20 column 40 foreground-color 01 background-color 02
             value "-Fim".
          02 line 20 column 47 foreground-color 07 background-color 02
             highlight value "PgDown".
          02 line 20 column 53 foreground-color 01 background-color 02
             value "-Prox".
          02 line 20 column 60 foreground-color 07 background-color 02
             highlight value "PgUp".
          02 line 20 column 64 foreground-color 01 background-color 02
             value "-Ant".
      *
       01 tela-05.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 07 background-color 02
             highlight value "1".
          02 line 20 column 07 foreground-color 01 background-color 02
             value "-Inventario".
          02 line 20 column 21 foreground-color 07 background-color 02
             highlight value "2".
          02 line 20 column 22 foreground-color 01 background-color 02
             value "-Entrada".
          02 line 20 column 33 foreground-color 07 background-color 02
             highlight value "3".
          02 line 20 column 34 foreground-color 01 background-color 02
             value "-Saida".
          02 line 20 column 43 foreground-color 07 background-color 02
             highlight value "4".
          02 line 20 column 44 foreground-color 01 background-color 02
             value "-Devolucao".
          02 line 20 column 56 foreground-color 07 background-color 02
             highlight value "5".
          02 line 20 column 57 foreground-color 01 background-color 02
             value "-Extorno".
      *
       01 tela-06.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-07.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 20 column 06 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 20 column 08 foreground-color 01 background-color 02
             value "-Cadastro".
      *
       01 tela-09.
          02 line 06 column 65 foreground-color 07 background-color 04
             highlight value "Inclusao".
      *
       01 tela-10.
          02 line 06 column 65 foreground-color 07 background-color 04
             highlight value "Consulta".
      *
       01 tela-mensagem-cad.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 20 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 20 column 05 foreground-color 04 background-color 04
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
           move 04 to box-lin.
           move 72 to box-col-f.
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
           perform sec-inclusao.
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
       rot-move-ce03.
           move codigo to ce03-codigo.
           move data-mov to ce03-data-mov.
           move movimento to ce03-movimento.
           move grupo to ce03-grupo in ce03-chave-1
                         ce03-grupo in ce03-chave-2.
           move produto to ce03-produto in ce03-chave-1
                           ce03-produto in ce03-chave-2.
           move cliente to ce03-cliente.
           move nf to ce03-nf.
           move quantidade to ce03-quantidade.
           move preco to ce03-preco.
           move obs to ce03-obs.
           move flag-atu to ce03-flag-atu.
           move flag-fat to ce03-flag-fat.
           move param-usr to ce03-usuario.
           move param-data to ce03-data.
      *
       rot-move-campos.
           move ce03-codigo to codigo.
           move ce03-data-mov to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move data-aux to data-mov.
           move data-disp to data-mov-disp.
           move ce03-movimento to movimento.
           move ce03-grupo in ce03-chave-1 to grupo.
           move ce03-produto in ce03-chave-1 to produto.
           move ce03-cliente to cliente cliente-disp.
           move ce03-nf to nf.
           move ce03-quantidade to quantidade.
           move ce03-preco to preco preco-disp preco-aux.
           move ce03-obs to obs.
           move ce03-flag-atu to flag-atu.
           move ce03-flag-fat to flag-fat.
           move ce03-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move ce03-usuario to cab-usuario.
      *
       rot-controle-atu.
           move high-values to ce03-chave-controle.
           perform rot-ponteiro.
           perform rot-le-ce03-lock.
           if erro not = 0
              move " Erro no registro de controle - ARQCE03A.DAT - Tecle
      -       " <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
           else
              move "S" to ce03-atualizacao
              rewrite reg-ce03
              unlock arqce03 record
           end-if.
      *
       rot-le-ce03.
           move 0 to erro.
           read arqce03 invalid key move 1 to erro.
           if ce03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ce03.
      *
       rot-le-ce03-1.
           move 0 to erro.
           read arqce03 key ce03-chave-1 invalid key move 1 to erro.
           if ce03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ce03-1.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqce03 key is equal ce03-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-ce03
           end-start.
      *
       rot-le-ce03-lock.
           move 0 to erro.
           read arqce03 next. 
           if ce03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ce03-lock
           end-if.
           read arqce03 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqce03 previous at end move 1 to erro.
           if ce03-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
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
       rot-open-cd01.
           move 0 to erro.
           if cd01-stat = "F"
              open i-o arqcd01
              if cd01-status not = "00"
                 move 
                 " Erro de abertura no ARQCD01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to cd01-stat
               end-if
           end-if.
      *
       rot-close-cd01.
           if cd01-stat = "A"
              close arqcd01
              move "F" to cd01-stat
           end-if.
      *
       rot-erro-leitura-cd01.
           move " Erro de leitura - ARQCD01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-cd01.
           move 0 to erro.
           read arqcd01 invalid key move 1 to erro.
           if cd01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-cd01
           end-if.
      *
       rot-inic-arquivo.
           perform lmp-data-mov thru lmp-flag-fat.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-data-mov thru lmp-flag-fat.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-display.
           perform rot-move-campos.
           move grupo to ce01-grupo.
           perform rot-le-ce01.
           if erro not = 0
              move "Grupo nao cadastrado" to dgrupo
              move spaces to unidade
           else
              move ce01-descricao-a to dgrupo
              move ce01-unidade to unidade
           end-if.
           move grupo to ce02-grupo.
           move produto to ce02-produto.
           perform rot-le-ce02.
           if erro not = 0
              move "Produto nao cadastrado" to dproduto
           else
              move ce02-descricao-a to dproduto
           end-if.
           move cliente to cd01-codigo.
           if cliente not = 0
              perform rot-le-cd01
              if erro not = 0
                 move "Codigo nao cadastrado" to dcliente
              else
                 move cd01-razao-social-a to dcliente
              end-if
           else
              move spaces to dcliente
           end-if.
           perform dsp-data-mov thru dsp-flag-fat.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-pesq-cliente.
           perform rot-close-cd01.
           move 08 to rotina-col-cod.
           move 12 to rotina-lin-cod.
           move "3" to rotina-borda-cod.
           move spaces to rotina-fundo-cod.
           move "S" to rotina-sombra-cod.
           call "rotcd01" using param-menu campo-rotina-cod.
           cancel "rotcd01".
           perform rot-open-cd01.
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
       err-fatura.
           move " Movimento ja faturado - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
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
       err-codigo-n-c.
           move " Codigo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
      *  Sequencia para dar Accept
      *
       acc-data-mov.
           accept data-mov at 0722 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-movimento.
           accept movimento at 0822 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-grupo.
           accept grupo at 0922 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-unidade.
           accept unidade at 1431 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-produto.
           accept produto at 1022 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-cliente.
           accept cliente at 1122 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-nf.
           accept nf at 1322 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-quantidade.
           accept quantidade at 1422 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-preco.
           accept preco-aux at 1522 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-obs.
           accept obs at 1622 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-flag-atu.
           accept flag-atu at 1722 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-flag-fat.
           accept flag-fat at 1822 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-data-mov.
           display data-mov-disp at 0722 with foreground-color 15 
                   background-color 04.
      *
       dsp-movimento.
           display movimento at 0822 with foreground-color 15 
                   background-color 04.
      *
       dsp-grupo.
           display grupo at 0922 with foreground-color 15 
                   background-color 04.
           display dgrupo at 0928 with foreground-color 15 
                   background-color 04.
      *
       dsp-unidade.
           display unidade at 1431 with foreground-color 15 
                   background-color 04.
      *
       dsp-produto.
           display produto at 1022 with foreground-color 15 
                   background-color 04.
           display dproduto at 1028 with foreground-color 15 
                   background-color 04.
      *
       dsp-cliente.
           display cliente-disp at 1122 with foreground-color 15 
                   background-color 04.
           display dcliente at 1128 with foreground-color 15 
                   background-color 04.
      *
       dsp-nf.
           display nf at 1322 with foreground-color 15 
                   background-color 04.
      *
       dsp-quantidade.
           display quantidade at 1422 with foreground-color 15 
                   background-color 04.
      *
       dsp-preco.
           display preco-disp at 1522 with foreground-color 15 
                   background-color 04.
      *
       dsp-obs.
           display obs at 1622 with foreground-color 15 
                   background-color 04.
      *
       dsp-flag-atu.
           display flag-atu at 1722 with foreground-color 15 
                   background-color 04.
      *
       dsp-flag-fat.
           display flag-fat at 1822 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-data-mov.
           display limpa-10 at 0722 with foreground-color 15 
                   background-color 04.
      *
       lmp-movimento.
           display limpa-07 at 0822 with foreground-color 15 
                   background-color 04.
      *
       lmp-grupo.
           display limpa at 0922 with foreground-color 15 
                   background-color 04.
      *
       lmp-unidade.
           display limpa-10 at 1431 with foreground-color 15 
                   background-color 04.
      *
       lmp-produto.
           display limpa at 1022 with foreground-color 15 
                   background-color 04.
      *
       lmp-cliente.
           display limpa at 1122 with foreground-color 15 
                   background-color 04.
      *
       lmp-nf.
           display limpa at 1322 with foreground-color 15 
                   background-color 04.
      *
       lmp-quantidade.
           display limpa-07 at 1422 with foreground-color 15 
                   background-color 04.
      *
       lmp-preco.
           display limpa at 1522 with foreground-color 15 
                   background-color 04.
      *
       lmp-obs.
           display limpa at 1622 with foreground-color 15 
                   background-color 04.
      *
       lmp-flag-atu.
           display limpa-07 at 1722 with foreground-color 15 
                   background-color 04.
      *
       lmp-flag-fat.
           display limpa-07 at 1822 with foreground-color 15 
                   background-color 04.
      *
       sec-inclusao section.
      *
       lab-inc-00.
           display tela-limpa-cad.
           perform rot-open-ce01.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-ce02.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-ce03.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-cd01.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           if param-prioridade < 5
              perform sec-consulta
              go to lab-inc-fim
           end-if.
      *
       lab-inc-01.
           display tela-09.
           display tela-02.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move data-aux to data-mov.
           perform lmp-data-mov.
           perform acc-data-mov.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform lmp-grupo
              perform sec-consulta
              display tela-09
              go to lab-inc-01
           end-if.
           if data-mov = 0
              go to lab-inc-01
           end-if.           
           move data-mov to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-inc-01
           end-if.
           move data-disp to data-mov-disp.
           move dias-corr to data-mov.
           perform dsp-data-mov.
           display tela-limpa-cad.
      *
       lab-inc-02.
           display tela-05.
           move 0 to movimento.
           perform lmp-movimento.
           perform acc-movimento.
           if escape-key = 1
              perform lmp-movimento
              go to lab-inc-01
           end-if.
           if movimento not = 1 and 2 and 3 and 4 and 5
              go to lab-inc-02
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-03.
           move 0 to grupo.
           perform lmp-grupo thru lmp-unidade.
           perform acc-grupo.
           if escape-key = 1
              perform lmp-grupo thru lmp-unidade
              go to lab-inc-02
           end-if.
           if grupo = 0
              go to lab-inc-03
           end-if.
           move grupo to ce01-grupo.
           perform rot-le-ce01.
           if erro not = 0
              perform err-grupo-n-c
              go to lab-inc-03
           end-if.
           move ce01-descricao-a to dgrupo.
           move ce01-unidade to unidade.
           perform dsp-grupo thru dsp-unidade.
      *
       lab-inc-04.
           move 0 to produto.
           perform lmp-produto.
           perform acc-produto.
           if escape-key = 1
              perform lmp-produto
              go to lab-inc-03
           end-if.
           if produto = 0
              go to lab-inc-04
           end-if.
           move grupo to ce02-grupo.
           move produto to ce02-produto.
           perform rot-le-ce02.
           if erro not = 0
              perform err-produto-n-c
              go to lab-inc-04
           end-if.
           move ce02-descricao-a to dproduto.
           perform dsp-produto.
           if movimento = 5
              go to lab-inc-06
           end-if.
           move 0 to rotina-codigo-cod.
      *
       lab-inc-05.
           display tela-07.
           move rotina-codigo-cod to cliente.
           perform lmp-cliente.
           perform acc-cliente.
           if escape-key = 1
              perform lmp-cliente
              display tela-limpa-cad
              go to lab-inc-04
           end-if.
           if escape-key = 3
              perform rot-pesq-cliente
              go to lab-inc-05
           end-if.
           if cliente = 0
              go to lab-inc-05
           end-if.
           move cliente to cd01-codigo cliente-disp.
           perform rot-le-cd01.
           if erro not = 0
              perform err-codigo-n-c
              go to lab-inc-05
           end-if.
           move cd01-razao-social-a to dcliente.
           perform dsp-cliente.
           display tela-limpa-cad.
      *
       lab-inc-06.
           move spaces to nf.
           perform lmp-nf.
           perform acc-nf.
           if escape-key = 1
              perform lmp-nf
              if movimento = 5
                 go to lab-inc-04
              else
                 move 0 to rotina-codigo-cod
                 go to lab-inc-05
              end-if
           end-if.
           if param-prioridade not = 9 or movimento = 1
              if nf = spaces
                 go to lab-inc-06
              end-if
           end-if.
      *
        lab-inc-07.
           move 0 to quantidade.
           perform lmp-quantidade.
           perform acc-quantidade.
           if escape-key = 1
              perform lmp-quantidade
              go to lab-inc-06
           end-if.
           move quantidade to quantidade-aux.
           if quantidade-aux = 0
              go to lab-inc-07
           end-if.
           if movimento = 1 or 5
              move 0 to preco
              go to lab-inc-09
           end-if.
      *
       lab-inc-08.
           move 0 to preco-aux.
           perform lmp-preco.
           perform acc-preco.
           if escape-key = 1
              perform lmp-preco
              go to lab-inc-07
           end-if.
           move preco-aux to preco.
           if preco = 0
              go to lab-inc-08
           end-if.
           move preco to preco-disp.
           perform dsp-preco.
      *
       lab-inc-09.
           move spaces to obs.
           perform lmp-obs.
           perform acc-obs.
           if escape-key = 1
              perform lmp-obs
              if movimento = 1 or 5
                 go to lab-inc-07
              else
                 go to lab-inc-08
              end-if
           end-if.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-inc-10.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-09
           end-if.
           if resposta = "N"
              perform lmp-grupo thru lmp-flag-fat
              display tela-limpa-cad
              go to lab-inc-03
           else
              if resposta not = "S"
                 go to lab-inc-10
              end-if
           end-if.
      *
       lab-inc-11.
           move high-values to ce03-chave-controle.
           perform rot-ponteiro.
           perform rot-le-ce03-lock.
           if erro not = 0
              move " Erro no registro de controle - ARQCE03A.DAT - Tecle
      -       " <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              go to lab-inc-fim
           end-if.
           move ce03-sequencia to codigo.
           add 1 to ce03-sequencia codigo.
           rewrite reg-ce03.
           unlock arqce03 record.
      *
       lab-inc-12.
           move "N" to flag-atu flag-fat.
           perform rot-move-ce03.
           write reg-ce03 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQCE03A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           display tela-06.
           perform rot-keypress.
           display tela-limpa-cad.
           perform lmp-grupo thru lmp-flag-fat.
           go to lab-inc-03.
      *
       lab-inc-fim.
           perform rot-close-ce01.
           perform rot-close-ce02.
           perform rot-close-ce03.
           perform rot-close-cd01.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           display tela-limpa-cad.
           display tela-10.
           perform lmp-data-mov.
      *
       lab-cns-01.
           display tela-03.
           move 0 to kbd2.
           perform until kbd2 = 27
                   perform rot-keypress
                   evaluate true
                            when kbd2 = 68 or 100
                                 display tela-limpa-cad
                                 perform sec-consulta-data
                                 display tela-03
                            when kbd2 = 80 or 112
                                 display tela-limpa-cad
                                 perform sec-consulta-produto
                                 display tela-03
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-fim.
           display tela-limpa.
           exit.
      *
       sec-consulta-data section.
      *
       lab-cns-data-00.
           move 0 to data-mov.
           perform lmp-data-mov.
           perform acc-data-mov.
           if escape-key = 1
              perform lmp-data-mov
              go to lab-cns-data-fim
           end-if.
           if data-mov = 0
              go to lab-cns-data-00-0
           end-if.
           move data-mov to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-cns-data-00
           end-if.
           move dias-corr to data-mov.
      *
       lab-cns-data-00-0.
           move 0 to movimento.
           perform lmp-movimento.
           perform acc-movimento.
           if escape-key = 1
              perform lmp-movimento
              go to lab-cns-data-00
           end-if.
           display tela-04.
           move low-values to ce03-chave-1.
           move data-mov to ce03-data-mov.
           move movimento to ce03-movimento.
      *
       lab-cns-data-00-a.
           start arqce03 key is not less ce03-chave-1.
           go to lab-cns-data-03.
      *
       lab-cns-data-01.
           perform rot-le-anterior.
           if erro not = 0 or ce03-codigo = codigo
              perform rot-inic-arquivo
              start arqce03 key is not less ce03-chave-1
              move 1 to erro
              go to lab-cns-data-05
           end-if.
           if ce03-chave = high-values
              go to lab-cns-data-01
           end-if.
           if param-prioridade not = 9
              if ce03-nf = spaces
                 go to lab-cns-data-01
              end-if
           end-if.
           go to lab-cns-data-04.
      *
       lab-cns-data-02.
           start arqce03 key is less ce03-chave-1.
      *
       lab-cns-data-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ce03
              go to lab-cns-data-fim
           end-if.
           if ce03-chave = high-values
              perform rot-fim-arquivo
              start arqce03 key is not less ce03-chave-1
              move 1 to erro
              move 0 to codigo
              go to lab-cns-data-05
           end-if.
           if param-prioridade not = 9
              if ce03-nf = spaces
                 go to lab-cns-data-03
              end-if
           end-if.
      *
       lab-cns-data-04.
           perform rot-display.
      *
       lab-cns-data-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            if flag-fat = "S"
                               perform err-fatura
                            else
                               perform sec-alteracao
                            end-if
                            go to lab-cns-data-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-data-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-data-03
                    when kbd-aux = 73
                         go to lab-cns-data-01
                    when kbd-aux = 71
                         move low-values to ce03-chave-1
                         go to lab-cns-data-00-a
                    when kbd-aux = 79
                         move high-values to ce03-chave-1
                         go to lab-cns-data-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-data-05
           end-if.
           perform lmp-data-mov thru lmp-flag-fat.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-data-00.
      *
       lab-cns-data-fim.
           move zeros to campo-kbd.
           perform lmp-data-mov thru lmp-flag-fat.
           display tela-limpa.
           exit.
      *
       sec-consulta-produto section.
      *
       lab-cns-produto-00.
           move 0 to grupo.
           perform lmp-grupo.
           perform acc-grupo.
           if escape-key = 1
              perform lmp-grupo
              go to lab-cns-produto-fim
           end-if.
      *
       lab-cns-produto-00-0.
           move 0 to produto.
           perform lmp-produto.
           perform acc-produto.
           if escape-key = 1
              perform lmp-produto
              go to lab-cns-produto-00
           end-if.
           display tela-04.
           move low-values to ce03-chave-2.
           move grupo to ce03-grupo in ce03-chave-2.
           move produto to ce03-produto in ce03-chave-2.
      *
       lab-cns-produto-00-a.
           start arqce03 key is not less ce03-chave-2.
           go to lab-cns-produto-03.
      *
       lab-cns-produto-01.
           perform rot-le-anterior.
           if erro not = 0 or ce03-codigo = codigo
              perform rot-inic-arquivo
              start arqce03 key is not less ce03-chave-2
              move 1 to erro
              go to lab-cns-produto-05
           end-if.
           if ce03-chave = high-values
              go to lab-cns-produto-01
           end-if.
           if param-prioridade not = 9
              if ce03-nf = spaces
                 go to lab-cns-produto-01
              end-if
           end-if.
           go to lab-cns-produto-04.
      *
       lab-cns-produto-02.
           start arqce03 key is less ce03-chave-2.
      *
       lab-cns-produto-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ce03
              go to lab-cns-produto-fim
           end-if.
           if ce03-chave = high-values
              perform rot-fim-arquivo
              start arqce03 key is not less ce03-chave-2
              move 1 to erro
              move 0 to codigo
              go to lab-cns-produto-05
           end-if.
           if param-prioridade not = 9
              if ce03-nf = spaces
                 go to lab-cns-produto-03
              end-if
           end-if.
      *
       lab-cns-produto-04.
           perform rot-display.
      *
       lab-cns-produto-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            if flag-fat = "S"
                               perform err-fatura
                            else
                               perform sec-alteracao
                            end-if
                            go to lab-cns-produto-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-produto-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-produto-03
                    when kbd-aux = 73
                         go to lab-cns-produto-01
                    when kbd-aux = 71
                         move low-values to ce03-chave-2
                         go to lab-cns-produto-00-a
                    when kbd-aux = 79
                         move high-values to ce03-chave-2
                         go to lab-cns-produto-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-produto-05
           end-if.
           perform lmp-data-mov thru lmp-flag-fat.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-produto-00.
      *
       lab-cns-produto-fim.
           move zeros to campo-kbd.
           perform lmp-data-mov thru lmp-flag-fat.
           display tela-limpa.
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
           perform rot-le-ce03-lock.
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
           delete arqce03 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQCE03A.DAT - Tecle <Enter>
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
           unlock arqce03 record.
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
           perform rot-le-ce03-lock.
           perform rot-display.
      *
       lab-alt-01.
           perform lmp-data-mov.
           perform acc-data-mov.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           if data-mov = 0
              go to lab-alt-01
           end-if.           
           move data-mov to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-alt-01
           end-if.
           move data-disp to data-mov-disp.
           move dias-corr to data-mov.
           perform dsp-data-mov.
           display tela-limpa-cad.
      *
       lab-alt-02.
           display tela-05.
           perform acc-movimento.
           if escape-key = 1
              move data-mov to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-mov
              go to lab-alt-01
           end-if.
           if movimento not = 1 and 2 and 3 and 4 and 5
              go to lab-alt-02
           end-if.
           display tela-limpa-cad.
      *
       lab-alt-03.
           perform lmp-grupo thru lmp-unidade.
           perform acc-grupo.
           if escape-key = 1
              perform dsp-grupo thru dsp-unidade
              go to lab-alt-02
           end-if.
           if grupo = 0
              go to lab-alt-03
           end-if.
           move grupo to ce01-grupo.
           perform rot-le-ce01.
           if erro not = 0
              perform err-grupo-n-c
              go to lab-alt-03
           end-if.
           move ce01-descricao-a to dgrupo.
           move ce01-unidade to unidade.
           perform dsp-grupo thru dsp-unidade.
      *
       lab-alt-04.
           perform lmp-produto.
           perform acc-produto.
           if escape-key = 1
              perform dsp-produto
              go to lab-alt-03
           end-if.
           if produto = 0
              go to lab-alt-04
           end-if.
           move grupo to ce02-grupo.
           move produto to ce02-produto.
           perform rot-le-ce02.
           if erro not = 0
              perform err-produto-n-c
              go to lab-alt-04
           end-if.
           move ce02-descricao-a to dproduto.
           perform dsp-produto.
           if movimento = 5
              move 0 to cliente cliente-disp 
              move spaces to dcliente
              perform lmp-cliente
              go to lab-alt-06
           end-if.
           move cliente to rotina-codigo-cod.
      *
       lab-alt-05.
           display tela-07.
           move rotina-codigo-cod to cliente.
           perform lmp-cliente.
           perform acc-cliente.
           if escape-key = 1
              perform dsp-cliente
              display tela-limpa-cad
              go to lab-alt-04
           end-if.
           if escape-key = 3
              perform rot-pesq-cliente
              go to lab-alt-05
           end-if.
           if cliente = 0
              go to lab-alt-05
           end-if.
           move cliente to cd01-codigo cliente-disp.
           perform rot-le-cd01.
           if erro not = 0
              perform err-codigo-n-c
              go to lab-alt-05
           end-if.
           move cd01-razao-social-a to dcliente.
           perform dsp-cliente.
           display tela-limpa-cad.
      *
       lab-alt-06.
           perform lmp-nf.
           perform acc-nf.
           if escape-key = 1
              if movimento = 5
                 go to lab-alt-04
              else
                 go to lab-alt-05
              end-if
           end-if.
           if param-prioridade not = 9 or movimento = 1
              if nf = spaces
                 go to lab-alt-06
              end-if
           end-if.
      *
        lab-alt-07.
           perform acc-quantidade.
           if escape-key = 1
              go to lab-alt-06
           end-if.
           move quantidade to quantidade-aux.
           if quantidade-aux = 0
              go to lab-alt-07
           end-if.
           if movimento = 1 or 5
              move 0 to preco preco-disp preco-aux
              perform lmp-preco
              go to lab-alt-09
           end-if.
      *
       lab-alt-08.
           perform lmp-preco.
           perform acc-preco.
           if escape-key = 1
              move preco-aux to preco preco-disp
              perform dsp-preco
              go to lab-alt-07
           end-if.
           move preco-aux to preco.
           if preco = 0
              go to lab-alt-08
           end-if.
           move preco to preco-disp.
           perform dsp-preco.
      *
       lab-alt-09.
           perform acc-obs.
           if escape-key = 1
              if movimento = 1 or 5
                 go to lab-alt-07
              else
                 go to lab-alt-08
              end-if
           end-if.
      *
       lab-alt-10.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-09
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-10
              end-if
           end-if.
           if flag-atu = "S"
              move "N" to flag-atu
              perform rot-controle-atu
              if erro not = 0
                 go to lab-alt-fim
              end-if
           end-if.
           perform rot-move-ce03.
           rewrite reg-ce03 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQCE03A.DAT - Tecle <Ent
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
           unlock arqce03 record.
           display tela-04.
           exit.