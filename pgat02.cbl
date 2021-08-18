      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGAT02       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao do cadastro :                                      *
      *                                                             *
      *  Data da ultima alteracao:    14/09/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgat02.
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
          02 cb-programa               pic x(08) value "PGAT02".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 limpa                        pic x(20) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 linha                        pic 9(03) comp-5 value 0.
       01 coluna                       pic 9(04) comp-5 value 1.
       01 tamanho                      pic 9(04) comp-5 value 78.
       01 pagina                       pic 9(03) value 0.
       01 tracos-i                     pic x(80) value all "-".
       01 total                        pic 9(05) value 0.
      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
          02 sele-uf                   pic x(02) value spaces.
          02 sele-uf-disp              pic x(05) value spaces.
          02 sele-atividade            pic 9(03) value 0.
          02 sele-atividade-disp       pic x(05) value spaces.
      *
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 campo-rotina.
          02 rotina-col                pic 9(02) value 0.
          02 rotina-lin                pic 9(02) value 0.
          02 rotina-borda              pic x(01) value spaces.
          02 rotina-fundo              pic x(01) value spaces.
          02 rotina-sombra             pic x(01) value spaces.
          02 rotina-tipo               pic 9(02) value 0.
          02 rotina-codigo             pic 9(03) value 0.
      *
       01 campo-rotina-uf.
          02 rotina-col-uf             pic 9(02) value 0.
          02 rotina-lin-uf             pic 9(02) value 0.
          02 rotina-borda-uf           pic x(01) value spaces.
          02 rotina-fundo-uf           pic x(01) value spaces.
          02 rotina-sombra-uf          pic x(01) value spaces.
          02 rotina-tipo-uf            pic 9(02) value 0.
          02 rotina-uf                 pic x(02) value spaces.
      *
       01 cab-abav.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(01) value x"0e".
          02 filler                    pic x(08) value "ABAV/CN ".
          02 filler                    pic x(01) value x"14".
          02 filler                    pic x(01) value x"0f".
          02 filler                    pic x(51) value
          "Associacao Brasileira de Agencias de Viagens - CN".
          02 filler                    pic x(01) value x"12".
      *
       01 cab-prog.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(36) value
          "Relacao de Associadas por Atividade".
          02 filler                    pic x(17) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-01.
          02 filler                    pic x(11) value "Atividade:".
          02 cab-Atividade             pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(05) value "U.F.:".
          02 filler                    pic x(01) value spaces.
          02 cab-estado                pic x(20) value spaces.
      *
       01 cab-03.
          02 filler                    pic x(11) value "Empresa..:".
          02 cab-empresa               pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(11) value "Codigo...:".
          02 cab-codigo                pic x(06) value spaces.
      *
       01 cab-04.
          02 filler                    pic x(11) value "Diretor..:".
          02 cab-diretor               pic x(40) value spaces.
      *
       01 cab-05.
          02 filler                    pic x(11) value "Endereco.:".
          02 cab-endereco              pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(06) value "U.F.:".
          02 cab-uf                    pic x(02) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(08) value "C.E.P.:".
          02 cab-cep                   pic 9(05)b9(03) value 0
             blank when zero.
      *
       01 cab-06.
          02 filler                    pic x(11) value "Cidade...:".
          02 cab-cidade                pic x(15) value spaces.
          02 filler                    pic x(26) value spaces.
          02 filler                    pic x(11) value "D.D.D....:".
          02 cab-ddd                   pic 9(04) value 0
             blank when zero.
      *
       01 cab-07.
          02 filler                    pic x(11) value "Fone.....:".
          02 cab-telefone              pic x(08) value spaces.
          02 filler                    pic x(33) value spaces.
          02 filler                    pic x(11) value "Telex....:".
          02 cab-telex                 pic x(09) value spaces.
      *
       01 cab-08.
          02 filler                    pic x(11) value "Fax......:".
          02 cab-fax                   pic x(08) value spaces.
          02 filler                    pic x(08) value spaces.
      *
       01 cab-tot.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(05) value "Total".
          02 filler                    pic x(65) value spaces.
          02 cab-total                 pic 9(05) value 0.
          02 filler                    pic x(01) value spaces.
      *
       copy wstab01.lib.
       copy wstab03.lib.
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
          02 line 13 column 39 foreground-color 07 background-color 04
             highlight value "Relacao".
          02 line 14 column 06 foreground-color 06 background-color 04
             value "Ordenamento...:".
          02 line 15 column 06 foreground-color 06 background-color 04
             value "U.F...........:".
          02 line 16 column 06 foreground-color 06 background-color 04
             value "Atividade.....:".
      *
       01 tela-02.
          02 line 18 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 18 column 08 foreground-color 07 background-color 02
             highlight value "1".
          02 line 18 column 09 foreground-color 01 background-color 02
             value "-Codigo".
          02 line 18 column 19 foreground-color 07 background-color 02
             highlight value "2".
          02 line 18 column 20 foreground-color 01 background-color 02
             value "-Empresa".
          02 line 18 column 30 foreground-color 07 background-color 02
             highlight value "3".
          02 line 18 column 31 foreground-color 01 background-color 02
             value "-Diretor".
      *
       01 tela-03.
          02 line 18 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 18 column 06 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 18 column 08 foreground-color 01 background-color 02
             value " - Estados".
      *
       01 tela-04.
          02 line 18 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 18 column 06 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 18 column 08 foreground-color 01 background-color 02
             value " - Atividades".
      *
       01 tela-08.
          02 line 18 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 18 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 18 column 12 foreground-color 07 background-color 02
             highlight value "C".
          02 line 18 column 13 foreground-color 01 background-color 02
             value ")ontinuar   (".
          02 line 18 column 26 foreground-color 07 background-color 02
             highlight value "F".
          02 line 18 column 27 foreground-color 01 background-color 02
             value ")inalizar".
      *
       01 tela-09.
          02 line 18 column 05 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 18 column 05 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 18 column 12 foreground-color 07 background-color 02
             highlight value "I".
          02 line 18 column 13 foreground-color 01 background-color 02
             value ") para interromper".
      *
       01 tela-mensagem-cad.
          02 line 18 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from mensagem.
      *
       01 tela-erro-cad.
          02 line 18 column 05 beep reverse-video pic x(41) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 18 column 05 foreground-color 04 background-color 04
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
           move 18 to box-lin-f.
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
       rot-pesq-tabela.
           perform rot-close-tabl.
           move 08 to rotina-col.
           move 09 to rotina-lin.
           move "3" to rotina-borda.
           move spaces to rotina-fundo.
           move "S" to rotina-sombra.
           call "pgtab01" using param-menu campo-rotina.
           cancel "pgtab01".
           perform rot-open-tabl.
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
              go to rot-le-tabl.
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
           move 9 to linha.
           add 1 to pagina
           move pagina to cab-pagina
           if pagina = 1
              write reg-imp from cab-abav
              write reg-imp from cab-prog after 1 line
           else
              write reg-imp from cab-abav after page
              write reg-imp from cab-prog after 2 lines
           end-if.
           write reg-imp from spaces after 1 line.
           write reg-imp from tracos-i after 1 line.
           write reg-imp from cab-01 after 1 line.
           write reg-imp from tracos-i after 1 line.
      *
       rot-move.
           move at01-codigo to cab-codigo.
           move at01-empresa-a to cab-empresa.
           move at01-diretor-a to cab-diretor.
           move at01-endereco to cab-endereco.
           move at01-cep to cab-cep.
           move at01-uf to cab-uf.
           move at01-cidade to cab-cidade.
           move at01-ddd to cab-ddd.
           move at01-telefone to cab-telefone.
           move at01-telex to cab-telex.
           move at01-fax to cab-fax.
           add 1 to total.
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
       err-atividade.
           move " Atividade nao cadastrada - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-04.
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
       acc-ord.
           accept sele-ord at 1422 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept sele-uf at 1522 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-atividade.
           accept sele-atividade at 1622 with auto update prompt
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
       dsp-uf.
           display sele-uf-disp at 1522 with foreground-color 15 
                   background-color 04.
      *
       dsp-atividade.
           display sele-atividade-disp at 1622 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1422 with foreground-color 15 
                   background-color 04.
      *
       lmp-uf.
           display limpa at 1522 with foreground-color 15 
                   background-color 04.
      *
       lmp-atividade.
           display limpa at 1622 with foreground-color 15 
                   background-color 04.
      *
       sec-selecao section.
      *
       lab-sele-00.
           perform rot-open-tabl.
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
           if sele-ord not = 1 and 2 and 3
              go to lab-sele-01
           end-if.
           move space to rotina-uf.
      *
       lab-sele-02.
           display tela-03.
           move rotina-uf to sele-uf.
           perform lmp-uf.
           perform acc-uf.
           if escape-key = 1
              perform lmp-uf
              go to lab-sele-01
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-sele-02
           end-if.           
           move sele-uf to txt.
           perform rot-texto.
           move txt to sele-uf.
           if txt = spaces
              move "Todos" to sele-uf-disp cab-estado
              perform dsp-uf
              go to lab-sele-03
           end-if.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move sele-uf to wtab03-sigla sele-uf-disp.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado
              go to lab-sele-02
           end-if.
           move reg-tabl to reg-wtab03.
           move wtab03-descricao to cab-estado.
           perform dsp-uf.
           display tela-limpa-cad.
           move 0 to rotina-codigo.
      *
       lab-sele-03.
           display tela-04.
           move 0 to sele-atividade
           perform lmp-atividade.
           perform acc-atividade.
           if escape-key = 1
              perform lmp-atividade
              go to lab-sele-02
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-sele-03
           end-if.
           if sele-atividade = 0
              move "Todas" to sele-atividade-disp cab-atividade
              perform dsp-atividade
              go to lab-sele-04
           end-if.
           move 06 to wtab01-tipo.
           move sele-atividade to wtab01-codigo sele-atividade-disp.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-sele-03
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to cab-atividade.
           perform dsp-atividade.
      *
       lab-sele-04.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-03
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-ord thru lmp-atividade
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-04
              end-if
           end-if.
           display tela-limpa-cad.
           perform sec-impressao.
           perform lmp-ord thru lmp-atividade.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform lmp-ord thru lmp-atividade.
           perform rot-close-tabl.
           exit.
      *
       sec-impressao section.
      *
       lab-imp-00.
           perform rot-open-at01.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           move 99 to linha.
           move 0 to pagina total.
           evaluate true
                  when sele-ord = 1
                       move low-values to at01-chave
                       start arqat01 key is not less at01-chave
                  when sele-ord = 2
                       move low-values to at01-chave-1
                       start arqat01 key is not less at01-chave-1
                  when sele-ord = 3
                       move low-values to at01-chave-2
                       start arqat01 key is not less at01-chave-2
           end-evaluate.
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
           if sele-atividade not = 0
              if sele-atividade not = at01-atv (01) and at01-atv (02)
                                  and at01-atv (03) and at01-atv (04) 
                                  and at01-atv (05)
                 go to lab-imp-01
              end-if
           end-if.
           if linha > 52
              perform rot-cabec
           end-if.
           perform rot-move.
           write reg-imp from cab-03 after 1 line.
           write reg-imp from cab-04 after 1 line.
           write reg-imp from cab-05 after 1 line.
           write reg-imp from cab-06 after 1 line.
           write reg-imp from cab-07 after 1 line.
           write reg-imp from cab-08 after 1 line.
           write reg-imp from tracos-i after 1 line.
           add 7 to linha.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           if kbd2 not = 27 and pagina not = 0
              move total to cab-total
              write reg-imp from cab-tot after 1 line
              write reg-imp from tracos-i after 1 line
           end-if.
           if pagina not = 0
              write reg-imp from spaces after page
           end-if.
           perform rot-close-imp.
           perform rot-close-at01.
           exit.
      *