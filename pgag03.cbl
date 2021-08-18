      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGAG03       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Impressao de Relacao :                                     *
      *                                                             *
      *  Data da ultima alteracao:    23/06/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgag03.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqag01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ag01-chave
                  alternate record key is ag01-chave-1 with duplicates
                  alternate record key is ag01-chave-2 with duplicates
                  alternate record key is ag01-chave-3 with duplicates
                  alternate record key is ag01-chave-4 with duplicates
                  alternate record key is ag01-chave-5 with duplicates
                  file status is ag01-status.
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
           select arqobs01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is obs01-chave
                  file status is obs01-status.
      *
           select arqimp assign to printer
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdag01.lib.
      *
       copy fdtabl.lib.
      *
       copy fdobs01.lib.
      *
       fd arqimp

       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *    
       working-storage section.
      *
       01 ag01-status                  pic x(02) value "00".
       01 ag01-stat                    pic x(01) value "F".
      *
       01 nome-arq-ag01.
          02 ag01-dir                  pic x(03) value "AG2".
          02 filler                    pic x(01) value "\".
          02 ag01-nome-arq             pic x(08) value "ARQAG01A".
          02 filler                    pic x(01) value ".".
          02 ag01-ext                  pic x(03) value "DAT".
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
       01 obs01-status                 pic x(02) value "00".
       01 obs01-stat                   pic x(01) value "F".
      *
       01 nome-arq-obs01.
          02 obs01-dir                 pic x(03) value "USR".
          02 filler                    pic x(01) value "\".
          02 obs01-nome                pic x(08) value "ARQOBS01".
          02 filler                    pic x(01) value ".".
          02 obs01-ext                 pic x(03) value "DAT".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAG03".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(45) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 linha                        pic 9(03) comp-5 value 0.
       01 pagina                       pic 9(03) value 0.
       01 tracos-i                     pic x(80) value all "-".
       01 total                        pic 9(05) value 0.

      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
          02 sele-codigo-i             pic 9(05) value 0.
          02 sele-codigo-i-disp        pic x(10) value spaces.
          02 sele-dcodigo-i            pic x(40) value spaces.
          02 sele-codigo-f             pic 9(05) value 0.
          02 sele-codigo-f-disp        pic x(10) value spaces.
          02 sele-dcodigo-f            pic x(40) value spaces.
          02 sele-data-a               pic 9(06) value 0.
          02 sele-data-a-disp          pic x(08) value spaces.
          02 sele-uf                   pic x(02) value spaces.
          02 sele-uf-disp              pic x(05) value spaces.
          02 sele-posicao              pic x(01) value spaces.
          02 sele-posicao-disp         pic x(10) value spaces.
          02 sele-texto                pic x(40) value spaces.
          02 sele-observacao           pic x(01) value spaces.
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
          02 rotina-tipo-uf            pic 9(02) value 0.
          02 rotina-uf                 pic x(02) value spaces.
      *
       01 campo-string.
          02 string-a                  pic x(40) value spaces.
          02 filler                    pic x(01) value low-values.
      *
       01 campo-lixo.
         02 lixo                       pic x(1024) value spaces.
         02 filler                     pic x(01) value low-values.
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
          "Relacao do cadastro".
          02 filler                    pic x(17) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-02.
          02 filler                    pic x(11) value "Assunto..:".
          02 cab-assunto               pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(11) value "Codigo...:".
          02 cab-codigo                pic x(06) value spaces.
      *
       01 cab-03.
          02 filler                    pic x(11) value "Empresa..:".
          02 cab-empresa               pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(11) value "Partido..:".
          02 cab-partido               pic x(18) value spaces.
      *
       01 cab-04.
          02 filler                    pic x(11) value "Nome.....:".
          02 cab-nome                  pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(11) value "Data.....:".
          02 cab-data-a                pic x(08) value spaces.
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
          02 filler                    pic x(33) value spaces.
          02 filler                    pic x(11) value "Posicao..:".
          02 cab-posicao               pic x(09) value spaces.
      *
       01 cab-tot.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(05) value "Total".
          02 filler                    pic x(65) value spaces.
          02 cab-total                 pic 9(05) value 0.
          02 filler                    pic x(01) value spaces.
      *
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
          02 line 12 column 67 foreground-color 06 background-color 01
             highlight value "Relacao".
          02 line 13 column 11 foreground-color 06 background-color 01
             highlight value "Ordenamento....:".
          02 line 14 column 11 foreground-color 06 background-color 01
             highlight value "Codigo Inicial.:".
          02 line 15 column 11 foreground-color 06 background-color 01
             highlight value "Codigo Final...:".
          02 line 16 column 11 foreground-color 06 background-color 01
             highlight value "Data...........:".
          02 line 17 column 11 foreground-color 06 background-color 01
             highlight value "UF.............:".
          02 line 18 column 11 foreground-color 06 background-color 01
             highlight value "Posicao........:".
          02 line 19 column 11 foreground-color 06 background-color 01
             highlight value "Texto..........:".
          02 line 20 column 11 foreground-color 06 background-color 01
             highlight value "Observacoes....:".
      *
       01 tela-02.
          02 line 22 column 10 foreground-color 02 background-color 03
             highlight pic x(64) from spaces.
          02 line 22 column 10 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 22 column 12 foreground-color 05 background-color 03
             value " - Estados".
      *
       01 tela-03.
          02 line 22 column 10 foreground-color 02 background-color 03
             highlight pic x(64) from spaces.
          02 line 22 column 12 foreground-color 02 background-color 03 
             highlight value "S".
          02 line 22 column 13 foreground-color 05 background-color 03
             value " - Sim".
          02 line 22 column 23 foreground-color 02 background-color 03 
             highlight value "N".
          02 line 22 column 24 foreground-color 05 background-color 03
             value " - Nao".
      *
       01 tela-04.
          02 line 22 column 10 foreground-color 02 background-color 03
             highlight pic x(64) from spaces.
          02 line 22 column 12 foreground-color 02 background-color 03 
             highlight value "1".
          02 line 22 column 13 foreground-color 05 background-color 03
             value "-Codigo".
          02 line 22 column 22 foreground-color 02 background-color 03 
             highlight value "2".
          02 line 22 column 23 foreground-color 05 background-color 03
             value "-Assunto".
          02 line 22 column 33 foreground-color 02 background-color 03 
             highlight value "3".
          02 line 22 column 34 foreground-color 05 background-color 03
             value "-Empresa".
          02 line 22 column 44 foreground-color 02 background-color 03 
             highlight value "4".
          02 line 22 column 45 foreground-color 05 background-color 03
             value "-Nome".
          02 line 22 column 52 foreground-color 02 background-color 03 
             highlight value "5".
          02 line 22 column 53 foreground-color 05 background-color 03
             value "-Partido".
          02 line 22 column 63 foreground-color 02 background-color 03 
             highlight value "6".
          02 line 22 column 64 foreground-color 05 background-color 03
             value "-Data".
      *
       01 tela-08.
          02 line 22 column 10 foreground-color 02 background-color 03
             highlight pic x(64) from spaces.
          02 line 22 column 10 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 22 column 17 foreground-color 02 background-color 03
             highlight value "C".
          02 line 22 column 18 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 22 column 31 foreground-color 02 background-color 03
             highlight value "F".
          02 line 22 column 32 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-09.
          02 line 22 column 10 foreground-color 02 background-color 03
             highlight pic x(64) from spaces.
          02 line 22 column 10 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 22 column 17 foreground-color 02 background-color 03
             highlight value "I".
          02 line 22 column 18 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-mensagem-cad.
          02 line 22 column 10 foreground-color 07 background-color 01
             highlight pic x(64) from mensagem.
      *
       01 tela-erro-cad.
          02 line 22 column 10 beep reverse-video pic x(64) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 22 column 10 foreground-color 01 background-color 01
             pic x(64) from spaces.
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
           move 08 to box-col.
           move 10 to box-lin.
           move 73 to box-col-f.
           move 22 to box-lin-f.
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
           move ag01-codigo to cab-codigo.
           move ag01-assunto-a to cab-assunto.
           move ag01-empresa-a to cab-empresa.
           move ag01-nome-a to cab-nome.
           move ag01-partido-a to cab-partido.
           move ag01-data-a to dias-corr.
           move 1 to opcao-data. 
           perform rot-data.
           move data-disp to cab-data-a.
           move ag01-endereco to cab-endereco.
           move ag01-cep to cab-cep.
           move ag01-uf to cab-uf.
           move ag01-cidade to cab-cidade.
           move ag01-telefone to cab-telefone.
           move ag01-ddd to cab-ddd.
           move ag01-fax to cab-fax.
           move ag01-telex to cab-telex.
           move ag01-posicao to cab-posicao.
           add 1 to total.
           

      *
       rot-le-ag01.
           move 0 to erro.
           read arqag01 invalid key move 1 to erro.
           if ag01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ag01.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqag01 next at end move 1 to erro.
           if ag01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ag01.
           move 0 to erro.
           if ag01-stat = "F"
              open i-o arqag01
              if ag01-status not = "00"
                 move 
                 " Erro de abertura no ARQAG01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ag01-stat
               end-if
           end-if.
      *
       rot-close-ag01.
           if ag01-stat = "A"
              close arqag01
              move "F" to ag01-stat
           end-if.
      *
       rot-erro-leitura-ag01.
           move " Erro de leitura - ARQAG01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-obs01.
           move 0 to erro.
           read arqobs01 invalid key move 1 to erro.
           if obs01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-obs01
           end-if.
      *
       rot-open-obs01.
           move 0 to erro.
           if obs01-stat = "F"
              open i-o arqobs01
              if obs01-status not = "00"
                 move 
                 " Erro de abertura no ARQOBS01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to obs01-stat
               end-if
           end-if.
      *
       rot-close-obs01.
           if obs01-stat = "A"
              close arqobs01
              move "F" to obs01-stat
           end-if.
      *
       rot-erro-leitura-obs01.
           move " Erro de leitura - ARQOBS01A.DAT - Tecle <Enter>" to
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
       rot-pesq-tabela-3.
           perform rot-close-tabl.
           move 10 to rotina-col-uf.
           move 11 to rotina-lin-uf.
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
           write reg-imp from tracos-i after 1 line.
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
       err-codigo-n.
           move " Codigo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-estado-n.
            move " Estado nao cadastrado - Tecle <Enter>" to mensagem.
            display tela-erro-cad.
            perform rot-keypress.
            display tela-02.
      *
       err-data-i.
           move " Data invalida - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
      *  Sequencia para dar Accept
      *
       acc-ord.
           accept sele-ord at 1328 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-codigo-i.
           accept sele-codigo-i at 1428 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-codigo-f.
           accept sele-codigo-f at 1528 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-a.
           accept sele-data-a at 1628 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept sele-uf at 1728 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-posicao.
           accept sele-posicao at 1828 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-texto.
           accept sele-texto at 1928 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-observacao.
           accept sele-observacao at 2028 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 1328 with foreground-color 15 
                   background-color 01.
      *
       dsp-codigo-i.
           display sele-codigo-i-disp at 1428 with foreground-color 15 
                   background-color 01.
           if sele-dcodigo-i not = spaces
              display sele-dcodigo-i at 1434 with foreground-color 15 
                      background-color 01
           end-if.
      *
       dsp-codigo-f.
           display sele-codigo-f-disp at 1528 with foreground-color 15 
                   background-color 01.
           if sele-dcodigo-f not = spaces
              display sele-dcodigo-f at 1534 with foreground-color 15 
                      background-color 01
           end-if.
      *
       dsp-data-a.
           display sele-data-a-disp at 1628 with foreground-color 15 
                   background-color 01.
      *
       dsp-uf.
           display sele-uf-disp at 1728 with foreground-color 15 
                   background-color 01.
      *
       dsp-posicao.
           display sele-posicao-disp at 1828 with foreground-color 15 
                   background-color 01.
      *
       dsp-texto.
           display sele-texto at 1928 with foreground-color 15 
                   background-color 01.
      *
       dsp-observacao.
           display sele-observacao at 2028 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1328 with foreground-color 15 
                   background-color 01.
      *
       lmp-codigo-i.
           display limpa at 1428 with foreground-color 15 
                   background-color 01.
      *
       lmp-codigo-f.
           display limpa at 1528 with foreground-color 15 
                   background-color 01.
      *
       lmp-data-a.
           display limpa at 1628 with foreground-color 15 
                   background-color 01.
      *
       lmp-uf.
           display limpa at 1728 with foreground-color 15 
                   background-color 01.
      *
       lmp-posicao.
           display limpa at 1828 with foreground-color 15 
                   background-color 01.
      *
       lmp-texto.
           display limpa at 1928 with foreground-color 15 
                   background-color 01.
      *
       lmp-observacao.
           display limpa at 2028 with foreground-color 15 
                   background-color 01.
      *
       sec-selecao section.
      *
       lab-sele-00.
           display tela-limpa-cad.
           perform rot-open-tabl
           if erro not = 0
              go to lab-sele-fim
           end-if.
           perform rot-open-ag01.
           if erro not = 0
              go to lab-sele-fim
           end-if.
      *
       lab-sele-01.
           display tela-04.
           move 0 to sele-ord.
           perform lmp-ord thru lmp-observacao.
           perform acc-ord.
           if escape-key = 1
              go to lab-sele-fim
           end-if.
           if sele-ord not = 1 and 2 and 3 and 4 and 5 and 6
              go to lab-sele-01
           end-if.
           display tela-limpa-cad.
      *
       lab-sele-02.
           move 0 to sele-codigo-i.
           perform lmp-codigo-i.
           perform acc-codigo-i.
           if escape-key = 1 
              perform lmp-codigo-i
              go to lab-sele-01
           end-if.
           if escape-key = 5
              go to lab-sele-01
           end-if.
           if sele-codigo-i = 0
              move spaces to sele-dcodigo-i
              move "Inicial" to sele-codigo-i-disp
              perform dsp-codigo-i
              go to lab-sele-03
           end-if.
           move sele-codigo-i to ag01-codigo sele-codigo-i-disp.
           perform rot-le-ag01.
           if erro not = 0
              perform err-codigo-n
              go to lab-sele-02
           end-if.
           move ag01-nome-a to sele-dcodigo-i.
           perform dsp-codigo-i.
      *
       lab-sele-03.
           move 0 to sele-codigo-f.
           perform lmp-codigo-f.
           perform acc-codigo-f.
           if escape-key = 1 
              perform lmp-codigo-f
              go to lab-sele-02
           end-if.
           if escape-key = 5
              go to lab-sele-01
           end-if.
           if sele-codigo-f < sele-codigo-i
              go to lab-sele-03
           end-if.
           if sele-codigo-f = 0
              move 99999 to sele-codigo-f
              move spaces to sele-dcodigo-f
              move "Final" to sele-codigo-f-disp
              perform dsp-codigo-f
              go to lab-sele-04
           end-if.
           move sele-codigo-f to ag01-codigo sele-codigo-f-disp.
           perform rot-le-ag01.
           if erro not = 0
              perform err-codigo-n
              go to lab-sele-03
           end-if.
           move ag01-nome-a to sele-dcodigo-f.
           perform dsp-codigo-f.
      *
       lab-sele-04.
           move 0 to sele-data-a.
           perform lmp-data-a.
           perform acc-data-a.
           if escape-key = 1
              perform lmp-data-a
              go to lab-sele-03
           end-if.
           if escape-key = 5
              go to lab-sele-01
           end-if.
           if sele-data-a = 0
              move "Todas" to sele-data-a-disp
              perform dsp-data-a
              move spaces to rotina-uf
              go to lab-sele-05
           end-if.
           move sele-data-a to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-sele-04
           end-if.
           move data-disp to sele-data-a-disp.
           move dias-corr to sele-data-a
           perform dsp-data-a.
           move spaces to rotina-uf.
      *
       lab-sele-05.
           display tela-02.
           move rotina-uf to sele-uf.
           perform lmp-uf.
           perform acc-uf.
           if escape-key = 1
              perform lmp-uf
              display tela-limpa-cad
              go to lab-sele-04
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-sele-05
           end-if.
           if escape-key = 5
              go to lab-sele-01
           end-if.
           move sele-uf to txt.
           perform rot-texto.
           if txt = spaces
              move "Todas" to sele-uf-disp
              perform dsp-uf
              display tela-limpa-cad
              go to lab-sele-06
           end-if.
           move txt to sele-uf sele-uf-disp.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move sele-uf to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado-n
              go to lab-sele-05
           end-if.
           display tela-limpa-cad.
      *
       lab-sele-06.
           display tela-03.
           move spaces to sele-posicao.
           perform lmp-posicao.
           perform acc-posicao.
           if escape-key = 1
              move spaces to rotina-uf
              perform lmp-posicao
              go to lab-sele-05
           end-if.
           if escape-key = 5
              go to lab-sele-01
           end-if.
           move sele-posicao to txt.
           perform rot-texto.
           if txt = spaces
              move "Todas" to sele-posicao-disp
              perform dsp-posicao
              display tela-limpa-cad
              go to lab-sele-07
           end-if.
           if txt not = "S" and "N"
              go to lab-sele-06
           end-if.
           move txt to sele-posicao sele-posicao-disp.
           perform dsp-posicao.
           display tela-limpa-cad.
      *
       lab-sele-07.
           move spaces to sele-texto.
           perform lmp-texto.
           perform acc-texto.
           if escape-key = 1
              perform lmp-texto
              go to lab-sele-06
           end-if.
           if escape-key = 5
              go to lab-sele-01
           end-if.
           if sele-texto = spaces
              move "Todos" to sele-texto
              perform dsp-texto
              move spaces to sele-texto
           else
              move sele-texto to string-a
              call "C_Strfim" using by reference campo-string
              call "C_strupr" using by reference campo-string
           end-if.
      *
       lab-sele-08.
           display tela-03.
           move spaces to sele-observacao.
           perform lmp-observacao.
           perform acc-observacao.
           if escape-key = 1
              display tela-limpa-cad
              perform lmp-observacao
              go to lab-sele-07
           end-if.
           if escape-key = 5
              go to lab-sele-01
           end-if.
           move sele-observacao to txt.
           perform rot-texto.
           if txt not = "S" and "N"
              go to lab-sele-08
           end-if.
           move txt to sele-observacao.
           perform dsp-observacao.
           display tela-limpa-cad.
      *
       lab-sele-09.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-08
           end-if.
           if resposta = "N"
              perform lmp-ord thru lmp-observacao
              display tela-limpa-cad
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-08
              end-if
           end-if.
           perform sec-impressao.
           perform lmp-ord thru lmp-observacao.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform rot-close-ag01.
           perform rot-close-tabl.
           exit.
      *
       sec-impressao section.
      *
       lab-imp-00.
           if sele-texto not = spaces
              perform rot-open-obs01
              if erro not = 0
                 go to lab-imp-fim
              end-if
           end-if.
           perform rot-open-obs01.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           move 99 to linha.
           evaluate true
                  when sele-ord = 1
                       move low-values to ag01-chave
                       start arqag01 key is not less ag01-chave
                  when sele-ord = 2
                       move low-values to ag01-chave-1
                       start arqag01 key is not less ag01-chave-1
                  when sele-ord = 3
                       move low-values to ag01-chave-2
                       start arqag01 key is not less ag01-chave-2
                  when sele-ord = 4
                       move low-values to ag01-chave-3
                       start arqag01 key is not less ag01-chave-3
                  when sele-ord = 5
                       move low-values to ag01-chave-4
                       start arqag01 key is not less ag01-chave-4
                  when sele-ord = 6
                       move low-values to ag01-chave-5
                       start arqag01 key is not less ag01-chave-5
           end-evaluate.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if ag01-chave = high-values
              go to lab-imp-01
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if ag01-codigo < sele-codigo-i or ag01-codigo > sele-codigo-f
              go to lab-imp-01
           end-if.
           if sele-data-a not = 0
              if ag01-data-a not = sele-data-a
                 go to lab-imp-01
              end-if
           end-if.
           if sele-uf not = spaces
              if ag01-uf not = sele-uf
                 go to lab-imp-01
              end-if
            end-if.
           if sele-posicao not = spaces
              if ag01-posicao not = sele-posicao
                 go to lab-imp-01
              end-if
           end-if.
           if sele-texto not = spaces
               perform sec-localiza
               if return-code = 0
                  go to lab-imp-01
               end-if
           end-if.
      *
       lab-imp-02.
           if linha > 52
              perform rot-cabec
           end-if.
           perform rot-move.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-03 after 1 line.
           write reg-imp from cab-04 after 1 line.
           write reg-imp from cab-05 after 1 line.
           write reg-imp from cab-06 after 1 line.
           write reg-imp from cab-07 after 1 line.
           write reg-imp from cab-08 after 1 line.
           write reg-imp from tracos-i after 1 line.
           if sele-observacao = "S"
              perform sec-imprime-obs
           end-if.
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
           perform rot-close-obs01.
           exit.
      *
       sec-localiza section.
      *
       lab-loc-00.
           move reg-ag01 to lixo.
           call "C_strupr" using by reference campo-lixo.
           call "C_Strloc" using by reference campo-lixo
                                 by reference campo-string.
           if return-code = 1
              go to lab-loc-fim
           end-if.
           move ag01-codigo to obs01-codigo
           move 01 to obs01-arquivo.
           perform rot-le-obs01.
           if erro = 0
              move reg-obs01 to lixo
              call "C_strupr" using by reference campo-lixo
              call "C_Strloc" using by reference campo-lixo
                                    by reference campo-string
           end-if.
      *
       lab-loc-fim.
           exit.
      *
       sec-imprime-obs section.
      *
       lab-imp-obs-00.
           write reg-imp from "O B S E R V A C O E S" after 1 line.
           write reg-imp from tracos-i after 1 line.
           move ag01-codigo to obs01-codigo
           move 01 to obs01-arquivo.
           perform rot-le-obs01.
           if erro = 0
              write reg-imp from obs01-obs (01) after 1 line
              write reg-imp from obs01-obs (02) after 1 line
              write reg-imp from obs01-obs (03) after 1 line
              write reg-imp from obs01-obs (04) after 1 line
              write reg-imp from obs01-obs (05) after 1 line
              write reg-imp from obs01-obs (06) after 1 line
              write reg-imp from obs01-obs (07) after 1 line
              write reg-imp from obs01-obs (08) after 1 line
              write reg-imp from obs01-obs (09) after 1 line
              write reg-imp from obs01-obs (10) after 1 line
              write reg-imp from obs01-obs (11) after 1 line
              write reg-imp from obs01-obs (12) after 1 line
              write reg-imp from obs01-obs (13) after 1 line
              write reg-imp from tracos-i after 1 line
           else
              write reg-imp from spaces after 12 line
              write reg-imp from tracos-i after 1 line
           end-if.
           add 20 to linha.
      *
       lab-imp-obs-fim.
           exit.
