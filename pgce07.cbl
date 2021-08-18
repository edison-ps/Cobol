      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGCE07       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao/Consulta de Movimentos :                           *
      *                                                             *
      *  Data da ultima alteracao:    05/06/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgce07.
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
           select arqimp assign to disk
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
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
       fd arqimp
      
       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
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
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGCE07".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(50) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-empresa                 pic x(01) value "N".
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 linha                        pic 9(03) comp-5 value 0.
       01 linha-detalhe                pic x(78) value spaces.
       01 coluna                       pic 9(04) comp-5 value 1.
       01 tamanho                      pic 9(04) comp-5 value 78.
       01 pagina                       pic 9(03) value 0.
       01 tracos-c                     pic x(78) value all "Ä".
       01 tracos-i                     pic x(78) value all "-".
       01 grupo-ant                    pic 9(05) value 0.
      *
       01 buffer2.
          02 filler                    pic 9(04) occurs 2000.
      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
          02 sele-movimento            pic 9(01) value 0.
          02 sele-movimento-disp       pic x(05) value spaces.
          02 sele-grupo                pic 9(05) value 0.
          02 sele-dgrupo               pic x(40) value spaces.
          02 sele-grupo-disp           pic x(05) value spaces.
          02 sele-produto              pic 9(05) value 0.
          02 sele-dproduto             pic x(40) value spaces.
          02 sele-produto-disp         pic x(05) value spaces.
          02 sele-cliente              pic 9(05) value 0.
          02 sele-cliente-disp         pic x(05) value spaces.
          02 sele-dcliente             pic x(40) value spaces.
          02 sele-device               pic 9(01) value 0.
      *
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 campo-rotina-cod.
          02 rotina-col-cod            pic 9(02) value 0.
          02 rotina-lin-cod            pic 9(02) value 0.
          02 rotina-borda-cod          pic x(01) value spaces.
          02 rotina-fundo-cod          pic x(01) value spaces.
          02 rotina-sombra-cod         pic x(01) value spaces.
          02 rotina-codigo-cod         pic 9(05) value 0.
      *
       01 cab-cabec.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(01) value x"0e".
          02 filler                    pic x(08) value "DISPASA ".
          02 filler                    pic x(01) value x"14".
          02 filler                    pic x(01) value x"0f".
          02 filler                    pic x(23) value
          "Dispasa Plasticos LTDA.".
          02 filler                    pic x(01) value x"12".
      *
       01 cab-prog.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(21) value
          "Relacao de Produtos".
          02 filler                    pic x(34) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-01.
          02 filler                    pic x(08) value spaces.
          02 filler                    pic x(07) value "Produto".
          02 filler                    pic x(05) value spaces.
          02 filler                    pic x(09) value "Descricao".
          02 filler                    pic x(47) value spaces.
      *
       01 cab-02.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(07) value "Estoque".
          02 filler                    pic x(05) value spaces.
          02 filler                    pic x(06) value "Minimo".
          02 filler                    pic x(07) value spaces.
          02 filler                    pic x(06) value "Maximo".
          02 filler                    pic x(07) value spaces.
          02 filler                    pic x(05) value "Medio".
          02 filler                    pic x(08) value spaces.
          02 filler                    pic x(04) value "Real".
          02 filler                    pic x(20) value spaces.
      *
       01 cab-03.
          02 filler                    pic x(01) value spaces.
          02 cab-grupo                 pic 9(05) value 0.
          02 filler                    pic x(02) value spaces.
          02 cab-descricao-g           pic x(40) value spaces.
          02 filler                    pic x(25) value spaces.
      *
       01 cab-04.
          02 filler                    pic x(08) value spaces.
          02 cab-produto               pic 9(05) value 0.
          02 filler                    pic x(07) value spaces.
          02 cab-descricao-p           pic x(40) value spaces.
          02 filler                    pic x(25) value spaces.
      *
       01 cab-05.
          02 filler                    pic x(11) value spaces.
          02 cab-minimo                pic -zzz.zzz value 0.
          02 filler                    pic x(05) value spaces.
          02 cab-maximo                pic -zzz.zzz value 0. 
          02 filler                    pic x(04) value spaces.
          02 cab-medio                 pic -zzz.zzz value 0.
          02 filler                    pic x(04) value spaces.
          02 cab-oficial               pic -zzz.zzz value 0.
          02 filler                    pic x(05) value spaces.
          02 cab-real                  pic -zzz.zzz value 0.
          02 filler                    pic x(10) value spaces.
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
          02 line 13 column 58 foreground-color 07 background-color 04
             highlight value "Consulta/Relacao".
          02 line 14 column 06 foreground-color 06 background-color 04
             value "Ordenamento...:".
          02 line 15 column 06 foreground-color 06 background-color 04
             value "Movimento.....:".
          02 line 16 column 06 foreground-color 06 background-color 04
             value "Grupo.........:".
          02 line 17 column 06 foreground-color 06 background-color 04
             value "Produto.......:".
          02 line 18 column 06 foreground-color 06 background-color 04
             value "Cliente.......:".
          02 line 19 column 06 foreground-color 06 background-color 04
             value "Device........:".
      *
       01 tela-02.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(69) from spaces.
          02 line 21 column 08 foreground-color 07 background-color 02
             highlight value "1".
          02 line 21 column 09 foreground-color 01 background-color 02
             value "-Codigo".
          02 line 21 column 25 foreground-color 07 background-color 02
             highlight value "2".
          02 line 21 column 26 foreground-color 01 background-color 02
             value "-Descricao".
      *
       01 tela-04.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(69) from spaces.
          02 line 21 column 08 foreground-color 07 background-color 02
             highlight value "1".
          02 line 21 column 09 foreground-color 01 background-color 02
             value "-Console".
          02 line 21 column 25 foreground-color 07 background-color 02
             highlight value "2".
          02 line 21 column 26 foreground-color 01 background-color 02
             value "-Impressora".
      *
       01 tela-05.
          02 line 23 column 02 foreground-color 01 background-color 02
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 23 column 09 foreground-color 07 background-color 02
             highlight value "C".
          02 line 23 column 10 foreground-color 01 background-color 02
             value ")ontinuar   (".
          02 line 23 column 23 foreground-color 07 background-color 02
             highlight value "F".
          02 line 23 column 24 foreground-color 01 background-color 02
             value ")inalizar".
      *
       01 tela-06.
          02 line 23 column 02 foreground-color 01 background-color 02
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 23 column 09 foreground-color 07 background-color 02
             highlight value "I".
          02 line 23 column 10 foreground-color 01 background-color 02
             value ") para interromper".
      *
       01 tela-07.
          02 line 23 column 02 foreground-color 01 background-color 02
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 01 background-color 02
             value "Tecle <Enter>".
      *
       01 tela-08.
          02 line 21 column 05 foreground-color 01 background-color 02
             pic x(69) from spaces.
          02 line 21 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 21 column 12 foreground-color 07 background-color 02
             highlight value "C".
          02 line 21 column 13 foreground-color 01 background-color 02
             value ")ontinuar   (".
          02 line 21 column 26 foreground-color 07 background-color 02
             highlight value "F".
          02 line 21 column 27 foreground-color 01 background-color 02
             value ")inalizar".
      *
       01 tela-09.
          02 line 21 column 05 foreground-color 01 background-color 02
             pic x(69) from spaces.
          02 line 21 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 21 column 12 foreground-color 07 background-color 02
             highlight value "I".
          02 line 21 column 13 foreground-color 01 background-color 02
             value ") para interromper".
      *
       01 tela-10.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(69) from spaces.
          02 line 21 column 06 foreground-color 07 background-color 02
             highlight value "1".
          02 line 21 column 07 foreground-color 01 background-color 02
             value "-Inventario".
          02 line 21 column 21 foreground-color 07 background-color 02
             highlight value "2".
          02 line 21 column 22 foreground-color 01 background-color 02
             value "-Entrada".
          02 line 21 column 33 foreground-color 07 background-color 02
             highlight value "3".
          02 line 21 column 34 foreground-color 01 background-color 02
             value "-Saida".
          02 line 21 column 43 foreground-color 07 background-color 02
             highlight value "4".
          02 line 21 column 44 foreground-color 01 background-color 02
             value "-Devolucao".
          02 line 21 column 56 foreground-color 07 background-color 02
             highlight value "5".
          02 line 21 column 57 foreground-color 01 background-color 02
             value "-Extorno".
      *
       01 tela-11.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(69) from spaces.
          02 line 21 column 06 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 21 column 08 foreground-color 01 background-color 02
             value "-Cadastro".
      *
       01 tela-mensagem-cad.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(69) from mensagem.
      *
       01 tela-erro-cad.
          02 line 21 column 05 beep reverse-video pic x(69) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 21 column 05 foreground-color 04 background-color 04
             pic x(69) from spaces.
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
           move 73 to box-col-f.
           move 21 to box-lin-f.
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
       rot-move.
           move ce02-grupo to cab-grupo ce01-grupo.
           perform rot-le-ce01.
           if erro = 0
              move ce01-descricao-a to cab-descricao-g
           else
              move "Grupo nao cadastrado" to cab-descricao-g
           end-if.
           move ce02-produto to cab-produto.
           move ce02-descricao-a to cab-descricao-p.
           move ce02-estoque-min to cab-minimo.
           move ce02-estoque-max to cab-maximo.
           move ce02-estoque-med to cab-medio.
           move ce02-estoque-ofic to cab-oficial.
           if param-prioridade = 9
              move ce02-estoque-real to cab-real
           end-if.
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
       rot-le-proximo.
           move 0 to erro.
           read arqce02 next at end move 1 to erro.
           if ce02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-le-ce02.
           move 0 to erro.
           read arqce02 invalid key move 1 to erro.
           if ce02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ce02.
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
       rot-interrompe-console.
           call "C_Readkey".
           move return-code to campo-kbd.
           if kbd2 = 73 or 105
              display tela-05
              perform until kbd2 = 67 or 99 or 70 or 102
                      perform rot-keypress
              end-perform
             if kbd2 = 70 or 102
                move "F" to resposta
             else
                display tela-06
             end-if
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
       rot-cabec.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move 7 to linha.
           add 1 to pagina
           move pagina to cab-pagina
           if pagina = 1
              write reg-imp from cab-cabec
              write reg-imp from cab-prog after 1 line
           else
              write reg-imp from cab-cabec after page
              write reg-imp from cab-prog after 2 lines
           end-if.
           write reg-imp from tracos-i after 1 line.
           write reg-imp from cab-01 after 1 line.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from tracos-i after 1 line.
      *
       rot-print.
           if linha not > 23
              call "C_Writexy" using by value coluna
                                     by value linha
                                     by value tamanho
                                     by value box-cor-f
                                     by value box-cor-p
                                     by reference linha-detalhe
              add 1 to linha
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
           accept resposta at 2040 with auto foreground-color 04
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
       acc-ord.
           accept sele-ord at 1422 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-movimento.
           accept sele-movimento at 1522 with auto update prompt
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
       acc-cliente.
           accept sele-cliente at 1822 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-device.
           accept sele-device at 1922 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 1422 with foreground-color 15 
                   background-color 04.
      *
       dsp-movimento.
           display sele-movimento-disp at 1522 with foreground-color 15 
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
       dsp-cliente.
           display sele-cliente-disp at 1822 with foreground-color 15 
                   background-color 04.
           display sele-dcliente at 1828 with foreground-color 15 
                   background-color 04.
      *
       dsp-device.
           display sele-device at 1922 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1422 with foreground-color 15 
                   background-color 04.
       lmp-movimento.
           display limpa at 1522 with foreground-color 15 
                   background-color 04.
      *
       lmp-grupo.
           display limpa at 1622 with foreground-color 15 
                   background-color 04.
      *
       lmp-produto.
           display limpa at 1722 with foreground-color 15 
                   background-color 04.
       lmp-cliente.
           display limpa at 1822 with foreground-color 15 
                   background-color 04.
      *
       lmp-device.
           display limpa at 1922 with foreground-color 15 
                   background-color 04.
      *
       sec-selecao section.
      *
       lab-sele-00.
           perform rot-open-ce01.
           if erro not = 0
              go to lab-sele-fim
           end-if.
           perform rot-open-ce02.
           if erro not = 0
              go to lab-sele-fim
           end-if.
           perform rot-open-cd01.
           if erro not = 0
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
           if sele-ord not = 1 and 2 
              go to lab-sele-01
           end-if.
           display tela-limpa-cad.
      *
       lab-sele-02.
           display tela-10.
           move 0 to sele-movimento.
           perform lmp-movimento.
           perform acc-movimento.
           if escape-key = 1
              perform lmp-movimento
              go to lab-sele-01
           end-if.
           if sele-movimento > 5
              go to lab-sele-02
           end-if.
           if sele-movimento = 0
              move "Todos" to sele-movimento-disp
           else
              move sele-movimento to sele-movimento-disp
           end-if.
           perform dsp-movimento.
           perform display tela-limpa-cad.
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
              move 0 to rotina-codigo-cod
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
              move 0 to rotina-codigo-cod
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
           move 0 to rotina-codigo-cod.
      *
       lab-sele-05.
           display tela-11.
           move rotina-codigo-cod to sele-cliente.
           perform lmp-cliente.
           perform acc-cliente.
           if escape-key = 1
              perform lmp-cliente
              display tela-limpa-cad
              if sele-grupo = 0
                 perform lmp-produto
                 go to lab-sele-03
              else
                 go to lab-sele-04
              end-if
           end-if.
           if escape-key = 3
              perform rot-pesq-cliente
              go to lab-sele-05
           end-if.
           if sele-cliente = 0
              move "Todos" to sele-cliente-disp
              move spaces to sele-dcliente
              perform dsp-cliente
              go to lab-sele-06
           end-if.
           move sele-cliente to cd01-codigo sele-cliente-disp.
           perform rot-le-cd01.
           if erro not = 0
              perform err-codigo-n-c
              go to lab-sele-05
           end-if.
           move cd01-razao-social-a to sele-dcliente.
           perform dsp-cliente.
           display tela-limpa-cad.
      *
       lab-sele-06.
           display tela-04.
           move 0 to sele-device.
           perform lmp-device.
           perform acc-device.
           if escape-key = 1
              perform lmp-device
              go to lab-sele-05
           end-if.
           if sele-device not = 1 and 2
              go to lab-sele-06
           end-if.
      *
       lab-sele-07.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-06
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-ord thru lmp-device
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-07
              end-if
           end-if.
           display tela-limpa-cad.
           if sele-device = 1
              perform sec-consulta
           else
              perform sec-impressao
           end-if.
           perform lmp-ord thru lmp-device.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform lmp-ord thru lmp-device.
           perform rot-close-cd01.
           perform rot-close-ce01.
           perform rot-close-ce02.
           exit.
      *
       sec-consulta section.
       sec-cns-00.
           move 01 to box-col.
           move 03 to box-lin.
           move 78 to box-col-f.
           move 22 to box-lin-f. 
           call "C_Savescr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
           display tela-cabec.
           move spaces to box-borda.
           move 00 to box-cor-f.
           move 15 to box-cor-p.
           move spaces to box-fundo.
           move "N" to box-sombra.
           perform rot-box.
           move 99 to linha.
           move 0 to grupo-ant.
           evaluate true
                  when sele-ord = 1
                       move low-values to ce02-chave
                       start arqce02 key is not less ce02-chave
                  when sele-ord = 2
                       move low-values to ce02-chave-1
                       start arqce02 key is not less ce02-chave-1
           end-evaluate.
      *
       lab-cns-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           if ce02-chave = high-values
              go to lab-cns-01
           end-if.
           perform rot-interrompe-console.
           if resposta = "F"
              move 27 to kbd2
              go to lab-cns-fim
           end-if.
           perform rot-move.
      *
       lab-cns-02.
           if linha > 19
              if linha not = 99
                 display tela-07
                 perform rot-keypress
                 if kbd2 = 27
                    go to lab-cns-fim
                 end-if
              end-if
              move 4 to linha
              perform rot-box
              display tela-06
              move cab-01 to linha-detalhe
              perform rot-print
              move cab-02 to linha-detalhe
              perform rot-print
              move tracos-c to linha-detalhe
              perform rot-print
              move 0 to grupo-ant
           end-if.
           if ce02-grupo not = grupo-ant
              if linha < 17
                 move ce02-grupo to grupo-ant
                 move cab-03 to linha-detalhe
                 perform rot-print
                 move spaces to linha-detalhe
                 perform rot-print
              else
                 add 4 to linha
                 go to lab-cns-02
              end-if
           end-if.
           move cab-04 to linha-detalhe.
           perform rot-print.
           move cab-05 to linha-detalhe.
           perform rot-print.
           go to lab-cns-01.
      * 
       lab-cns-fim.
           if kbd2 not = 27
              display tela-07
              perform rot-keypress
           end-if.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
           exit.
      *
       sec-impressao section.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           move 99 to linha.
           move 0 to pagina grupo-ant.
           evaluate true
                  when sele-ord = 1
                       move low-values to ce02-chave
                       start arqce02 key is not less ce02-chave
                  when sele-ord = 2
                       move low-values to ce02-chave-1
                       start arqce02 key is not less ce02-chave-1
           end-evaluate.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if ce02-chave = high-values
              go to lab-imp-01
           end-if.
           perform rot-interrompe-impressora.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if linha > 56
              perform rot-cabec
           end-if.
           perform rot-move.
           if ce02-grupo not = grupo-ant
              move ce02-grupo to grupo-ant
              write reg-imp from cab-03 after 1 line
              write reg-imp from spaces after 1 line
              add 2 to linha
           end-if.
           write reg-imp from cab-04 after 1 line.
           write reg-imp from cab-05 after 1 line.
           add 2 to linha.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           if pagina not = 0
              write reg-imp from spaces after page
           end-if.
           perform rot-close-imp.
           exit.
      *