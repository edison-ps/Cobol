      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGAT01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao do cadastro de AG. por atividades :             *
      *                                                             *
      *  Data da ultima alteracao:    13/09/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgat01.
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
       data division.
       file section.
      *    
       copy fdat01.lib.
      *
       copy fdtabl.lib.
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
          02 cb-programa               pic x(08) value "PGAT01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(50) value spaces.
       01 limpa-03                     pic x(03) value spaces.
       01 limpa-10                     pic x(10) value spaces.
       01 limpa-20                     pic x(20) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 codigo                    pic x(06) value spaces.
          02 empresa                   pic x(40) value spaces.
          02 empresa-a                 pic x(40) value spaces.
          02 diretor                   pic x(40) value spaces.
          02 diretor-a                 pic x(40) value spaces.
          02 endereco                  pic x(40) value spaces.
          02 cep                       pic 9(08) value 0.
          02 cidade                    pic x(15) value spaces.
          02 uf                        pic x(02) value spaces.
          02 ddd                       pic 9(04) value 0.
          02 telefone                  pic x(08) value spaces.
          02 telex                     pic x(08) value spaces.
          02 fax                       pic x(08) value spaces.
          02 atv-aux                   pic 9(03) value 0.
          02 atv-disp                  pic 9(03) value 0
                                       blank when zero.
          02 datv-disp                 pic x(40) value spaces.
          02 sub                       pic 9(02) value 0.
          02 sub-aux                   pic 9(02) value 0.
      *
       01 tab-atividade value zeros.
          02 atividade occurs 6 times.
             03 atv                    pic 9(03).
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
       01 cab-usr.
          02 filler                    pic x(10) value "Usuario.:".
          02 cab-usuario               pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Data....:".
          02 cab-data                  pic x(08) value spaces.
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
          02 line 09 column 06 foreground-color 06 background-color 04
             value "Codigo....:".
          02 line 10 column 06 foreground-color 06 background-color 04
             value "Empresa...:".
          02 line 11 column 06 foreground-color 06 background-color 04
             value "Diretor...:".
          02 line 13 column 06 foreground-color 06 background-color 04
             value "Endereco..:".
          02 line 14 column 06 foreground-color 06 background-color 04
             value "U.F.......:".
          02 line 14 column 30 foreground-color 06 background-color 04
             value "Cidade....:".
          02 line 15 column 06 foreground-color 06 background-color 04
             value "C.E.P.....:".
          02 line 16 column 06 foreground-color 06 background-color 04
             value "Telefone..:".
          02 line 16 column 30 foreground-color 06 background-color 04
             value "D.D.D.....:".
          02 line 17 column 06 foreground-color 06 background-color 04
             value "Fax.......:".
          02 line 18 column 06 foreground-color 06 background-color 04
             value "Telex.....:".
          02 line 19 column 06 foreground-color 06 background-color 04
             value "Atividade.:".
      *
       01 tela-02.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight value "F1".
          02 line 21 column 07 foreground-color 01 background-color 02
             value "-Help".
          02 line 21 column 15 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 21 column 17 foreground-color 01 background-color 02
             value "-Consultas".
      *
       01 tela-03.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 07 background-color 
             02 highlight value "F1".
          02 line 21 column 08 foreground-color 01 background-color 02
             value "-Help".
          02 line 21 column 16 foreground-color 07 background-color 02
             highlight value "C".
          02 line 21 column 17 foreground-color 01 background-color 02
             value "odigo".
          02 line 21 column 26 foreground-color 07 background-color 02
             highlight value "E".
          02 line 21 column 27 foreground-color 01 background-color 02
             value "mpresa".
         02 line 21 column 37 foreground-color 07 background-color 02
             highlight value "D".
          02 line 21 column 38 foreground-color 01 background-color 02
             value "iretor".
      *
       01 tela-04.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 07 background-color 02 
             highlight value "F2".
          02 line 21 column 08 foreground-color 01 background-color 02
             value "-Alt".
          02 line 21 column 15 foreground-color 07 background-color 02 
             highlight value "F3".
          02 line 21 column 17 foreground-color 01 background-color 02
             value "-Exc".
          02 line 21 column 25 foreground-color 07 background-color 02
             highlight value "Home".
          02 line 21 column 29 foreground-color 01 background-color 02
             value "-Inic".
          02 line 21 column 37 foreground-color 07 background-color 02
             highlight value "End".
          02 line 21 column 40 foreground-color 01 background-color 02
             value "-Fim".
          02 line 21 column 47 foreground-color 07 background-color 02
             highlight value "PgDown".
          02 line 21 column 53 foreground-color 01 background-color 02
             value "-Prox".
          02 line 21 column 60 foreground-color 07 background-color 02
             highlight value "PgUp".
          02 line 21 column 64 foreground-color 01 background-color 02
             value "-Ant".
      *
       01 tela-05.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 21 column 07 foreground-color 01 background-color 02
             value " - Atividade".
      *
       01 tela-06.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-08.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 21 column 07 foreground-color 01 background-color 02
             value " - Estados".
      *
       01 tela-09.
          02 line 08 column 65 foreground-color 07 background-color 04
             highlight value "Inclusao".
      *
       01 tela-10.
          02 line 08 column 65 foreground-color 07 background-color 04
             highlight value "Consulta".
      *
       01 tela-mensagem-cad.
          02 line 21 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 21 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 21 column 05 foreground-color 04 background-color 04
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
           move 06 to box-lin.
           move 72 to box-col-f.
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
       rot-move-at01.
           move codigo to at01-codigo.
           move empresa to at01-empresa.
           move empresa-a to at01-empresa-a.
           move diretor to at01-diretor.
           move diretor-a to at01-diretor-a.
           move endereco to at01-endereco.
           move uf to at01-uf.
           move cidade to at01-cidade.
           move cep to at01-cep.
           move telefone to at01-telefone.
           move ddd to at01-ddd.
           move fax to at01-fax.
           move telex to at01-telex.
           move atv (01) to at01-atv (01).
           move atv (02) to at01-atv (02).
           move atv (03) to at01-atv (03).
           move atv (04) to at01-atv (04).
           move atv (05) to at01-atv (05).
           move param-usr to at01-usuario.
           move param-data to at01-data.
      *
       rot-move-campos.
           move at01-codigo to codigo.
           move at01-empresa-a to empresa empresa-a.
           move at01-diretor-a to diretor diretor-a.
           move at01-endereco to endereco.
           move at01-uf to uf.
           move at01-cidade to cidade.
           move at01-cep to cep.
           move at01-telefone to telefone.
           move at01-ddd to ddd.
           move at01-fax to fax.
           move at01-telex to telex.
           move at01-atv (01) to atv (01).
           move at01-atv (02) to atv (02).
           move at01-atv (03) to atv (03).
           move at01-atv (04) to atv (04).
           move at01-atv (05) to atv (05).
           move at01-data to dias-corr.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move at01-usuario to cab-usuario.
      *
       rot-search.
           move 1 to erro.
           move 0 to sub.
           perform until sub = 6
                   add 1 to sub
                   if atv (sub) = atv-aux and sub-aux not = sub
                      move 0 to erro
                   end-if
           end-perform.
      *
       rot-le-at01.
           move 0 to erro.
           read arqat01 invalid key move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-at01.
      *
       rot-le-at01-1.
           move 0 to erro.
           read arqat01 key at01-chave-1 invalid key move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-at01-1.
      *
       rot-le-at01-2.
           move 0 to erro.
           read arqat01 key at01-chave-2 invalid key move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-at01-2.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqat01 key is equal at01-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-at01
           end-start.
      *
       rot-le-at01-lock.
           move 0 to erro.
           read arqat01 next. 
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-at01-lock
           end-if.
           read arqat01 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqat01 previous at end move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
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
                 move zeros to reg-at01
                 move high-values to at01-controle
                 write reg-at01-1
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
       rot-erro-leitura-at01.
           move " Erro de leitura - ARQAT01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-inic-arquivo.
           perform lmp-codigo thru lmp-atividade-05.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-codigo thru lmp-atividade-05.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
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
       rot-display.
           perform rot-move-campos.
           perform dsp-codigo thru dsp-atividade-05.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
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
       err-codigo-c.
           move " Codigo ja cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-atividade.
           move " Atividade nao cadastrada - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-05.
      *
       err-estado.
            move " Estado nao cadastrado - Tecle <Enter>" to mensagem.
            display tela-erro-cad.
            perform rot-keypress.
            display tela-08.
      *
      *  Sequencia para dar Accept
      *
       acc-codigo.
           accept codigo at 0918 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-empresa.
           accept empresa at 1018 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-diretor.
           accept diretor at 1118 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-endereco.
           accept endereco at 1318 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept uf at 1418 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-cidade.
           accept cidade at 1442 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-cep.
           accept cep at 1518 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-telefone.
           accept telefone at 1618 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-ddd.
           accept ddd at 1642 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-fax.
           accept fax at 1718 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-telex.
           accept telex at 1818 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-atividade-01.
           move atv (01) to atv-aux.
           accept atv-aux at 1918 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move atv-aux to atv (01).
           exit.
      *
       acc-atividade-02.
           move atv (02) to atv-aux.
           accept atv-aux at 1923 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move atv-aux to atv (02).
           exit.
      *
       acc-atividade-03.
           move atv (03) to atv-aux.
           accept atv-aux at 1928 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move atv-aux to atv (03).
           exit.
      *
       acc-atividade-04.
           move atv (04) to atv-aux.
           accept atv-aux at 1933 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move atv-aux to atv (04).
           exit.
      *
       acc-atividade-05.
           move atv (05) to atv-aux.
           accept atv-aux at 1938 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move atv-aux to atv (05).
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo.
           display codigo at 0918 with foreground-color 15 
                   background-color 04.
      *
       dsp-empresa.
           display empresa at 1018 with foreground-color 15 
                   background-color 04.
      *
       dsp-diretor.
           display diretor at 1118 with foreground-color 15 
                   background-color 04.
      *
       dsp-endereco.
           display endereco at 1318 with foreground-color 15 
                   background-color 04.
      *
       dsp-uf.
           display uf at 1418 with foreground-color 15 
                   background-color 04.
      *
       dsp-cidade.
           display cidade at 1442 with foreground-color 15 
                   background-color 04.
      *
       dsp-cep.
           display cep at 1518 with foreground-color 15 
                   background-color 04.
      *
       dsp-telefone.
           display telefone at 1618 with foreground-color 15 
                   background-color 04.
      *
       dsp-ddd.
           display ddd at 1642 with foreground-color 15 
                   background-color 04.
      *
       dsp-fax.
           display fax at 1718 with foreground-color 15 
                   background-color 04.
      *
       dsp-telex.
           display telex at 1818 with foreground-color 15 
                   background-color 04.
      *
       dsp-atividade-01.
           move atv (01) to atv-disp.
           display atv-disp at 1918 with foreground-color 15 
                   background-color 04.
           display datv-disp at 2018 with foreground-color 15 
                   background-color 04.
      *
       dsp-atividade-02.
           move atv (02) to atv-disp.
           display atv-disp at 1923 with foreground-color 15 
                   background-color 04.
           display datv-disp at 2018 with foreground-color 15 
                   background-color 04.
      *
       dsp-atividade-03.
           move atv (03) to atv-disp.
           display atv-disp at 1928 with foreground-color 15 
                   background-color 04.
           display datv-disp at 2018 with foreground-color 15 
                   background-color 04.
      *
       dsp-atividade-04.
           move atv (04) to atv-disp.
           display atv-disp at 1933 with foreground-color 15 
                   background-color 04.
           display datv-disp at 2018 with foreground-color 15 
                   background-color 04.
      *
       dsp-atividade-05.
           move atv (05) to atv-disp.
           display atv-disp at 1938 with foreground-color 15 
                   background-color 04.
           display datv-disp at 2018 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-codigo.
           display limpa at 0918 with foreground-color 15 
                   background-color 04.
      *
       lmp-empresa.
           display limpa at 1018 with foreground-color 15 
                   background-color 04.
      *
       lmp-diretor.
           display limpa at 1118 with foreground-color 15 
                   background-color 04.
      *
       lmp-endereco.
           display limpa at 1318 with foreground-color 15 
                   background-color 04.
      *
       lmp-uf.
           display limpa-03 at 1418 with foreground-color 15 
                   background-color 04.
      *
       lmp-cidade.
           display limpa-20 at 1442 with foreground-color 15 
                   background-color 04.
      *
       lmp-cep.
           display limpa at 1518 with foreground-color 15 
                   background-color 04.
      *
       lmp-telefone.
           display limpa-10 at 1618 with foreground-color 15 
                   background-color 04.
      *
       lmp-ddd.
           display limpa-10 at 1642 with foreground-color 15 
                   background-color 04.
      *
       lmp-fax.
           display limpa-10 at 1718 with foreground-color 15 
                   background-color 04.
      *
       lmp-telex.
           display limpa at 1818 with foreground-color 15 
                   background-color 04.
      *
       lmp-atividade-01.
           display limpa-03 at 1918 with foreground-color 15 
                   background-color 04.
           display limpa at 2018 with foreground-color 15 
                   background-color 04.
      *
       lmp-atividade-02.
           display limpa-03 at 1923 with foreground-color 15 
                   background-color 04.
           display limpa at 2018 with foreground-color 15 
                   background-color 04.
      *
       lmp-atividade-03.
           display limpa-03 at 1928 with foreground-color 15 
                   background-color 04.
           display limpa at 2018 with foreground-color 15 
                   background-color 04.
      *
       lmp-atividade-04.
           display limpa-03 at 1933 with foreground-color 15 
                   background-color 04.
           display limpa at 2018 with foreground-color 15 
                   background-color 04.
      *
       lmp-atividade-05.
           display limpa-03 at 1938 with foreground-color 15 
                   background-color 04.
           display limpa at 2018 with foreground-color 15 
                   background-color 04.
      *
       sec-inclusao section.
      *
       lab-inc-00.
           display tela-limpa-cad.
           perform rot-open-tabl
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-at01.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           display tela-09.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
      *
       lab-inc-01.
           display tela-09.
           display tela-02.
           move spaces to codigo datv-disp.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform sec-consulta
              go to lab-inc-01
           end-if.
           move codigo to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-01
           end-if.       
           move txt to codigo at01-codigo.
           perform dsp-codigo.
           perform rot-le-at01.
           if erro = 0
              perform err-codigo-c
              go to lab-inc-01
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-02.
           move spaces to empresa.
           perform lmp-empresa.
           perform acc-empresa.
           if escape-key = 1
              perform lmp-empresa
              go to lab-inc-01
           end-if.
           move empresa to txt empresa-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-02
           end-if.
           move txt to empresa.
      *
       lab-inc-03.
           move spaces to diretor.
           perform lmp-diretor.
           perform acc-diretor.
           if escape-key = 1
              perform lmp-diretor
              go to lab-inc-02
           end-if.
           move diretor to txt diretor-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-03
           end-if.
           move txt to diretor.
      *
       lab-inc-04.
           move spaces to endereco.
           perform lmp-endereco.
           perform acc-endereco.
           if escape-key = 1
              perform lmp-endereco
              go to lab-inc-03
           end-if.
           if endereco = spaces 
              go to lab-inc-04
           end-if.
           move spaces to rotina-uf.
      *
       lab-inc-05.
           display tela-08.
           move rotina-uf to uf.
           perform dsp-uf.
           perform acc-uf.
           if escape-key = 1
              perform lmp-uf
              display tela-limpa-cad
              go to lab-inc-04
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-inc-05
           end-if.           
           move uf to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-05
           end-if.
           move txt to uf.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move uf to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado
              go to lab-inc-05
           end-if.
           move reg-tabl to reg-wtab03.
           display tela-limpa-cad.
      *
       lab-inc-06.
           move wtab03-capital to cidade.
           perform lmp-cidade.
           perform acc-cidade.
           if escape-key = 1
              perform lmp-cidade
              go to lab-inc-05
           end-if.
           if cidade = spaces
              go to lab-inc-06
           end-if.
      *
       lab-inc-07.
           move 0 to cep.
           perform lmp-cep.
           perform acc-cep.
           if escape-key = 1
              perform lmp-cep
              go to lab-inc-06
           end-if.
           if cep = 0
              go to lab-inc-07
           end-if.
      *
       lab-inc-08.
           move spaces to telefone.
           perform lmp-telefone.
           perform acc-telefone.
           if escape-key = 1
              perform lmp-telefone
              go to lab-inc-07
           end-if.
           if telefone = spaces
              go to lab-inc-08
           end-if.
      *
       lab-inc-08-01.
           move wtab03-ddd to ddd.
           perform lmp-ddd.
           perform acc-ddd.
           if escape-key = 1
              perform lmp-ddd
              go to lab-inc-08
           end-if.
           if ddd = 0
              go to lab-inc-08-01
           end-if.
      *
       lab-inc-09.
           move spaces to fax.
           perform lmp-fax.
           perform acc-fax.
           if escape-key = 1
              perform lmp-fax
              go to lab-inc-08-01
           end-if.
      *
       lab-inc-10.
           move spaces to telex.
           perform lmp-telex.
           perform acc-telex.
           if escape-key = 1
              perform lmp-telex
              go to lab-inc-09
           end-if.
      *
       lab-inc-11.
           display tela-05.
           move 0 to atv (01).
           perform lmp-atividade-01.
           perform acc-atividade-01.
           if escape-key = 1
              perform lmp-atividade-01
              display tela-limpa-cad
              go to lab-inc-10
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-11
           end-if.
           if atv (01) = 0
              go to lab-inc-11
           end-if.
           move 06 to wtab01-tipo.
           move atv (01) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-inc-11
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to datv-disp.
           perform dsp-atividade-01.
           perform rot-keypress.
      *
       lab-inc-12.
           display tela-05.
           move 0 to atv (02).
           perform lmp-atividade-02.
           perform acc-atividade-02.
           if escape-key = 1
              perform lmp-atividade-02
              go to lab-inc-11
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-12
           end-if.
           if atv (02) = 0
              move 0 to atv (03) atv (04) atv (05)
              perform lmp-atividade-02
              go to lab-inc-16
           end-if.
           move 2 to sub-aux.
           perform rot-search.
           if erro = 0
              perform lmp-atividade-02
              go to lab-inc-12
           end-if.
           move 06 to wtab01-tipo.
           move atv (02) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-inc-12
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to datv-disp.
           perform dsp-atividade-02.
           perform rot-keypress.
      *
       lab-inc-13.
           display tela-05.
           move 0 to atv (03).
           perform lmp-atividade-03.
           perform acc-atividade-03.
           if escape-key = 1
              perform lmp-atividade-03
              go to lab-inc-12
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-13
           end-if.
           if atv (03) = 0
              move 0 to atv (04) atv (05)
              perform lmp-atividade-03
              go to lab-inc-16
           end-if.
           move 3 to sub-aux.
           perform rot-search.
           if erro = 0
              perform lmp-atividade-03
              go to lab-inc-13
           end-if.
           move 06 to wtab01-tipo.
           move atv (03) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-inc-13
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to datv-disp.
           perform dsp-atividade-03.
           perform rot-keypress.
      *
       lab-inc-14.
           display tela-05.
           move 0 to atv (04).
           perform lmp-atividade-04.
           perform acc-atividade-04.
           if escape-key = 1
              perform lmp-atividade-04
              go to lab-inc-13
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-14
           end-if.
           if atv (04) = 0
              move 0 to atv (05)
              perform lmp-atividade-04
              go to lab-inc-16
           end-if.
           move 4 to sub-aux.
           perform rot-search.
           if erro = 0
              perform lmp-atividade-04
              go to lab-inc-14
           end-if.
           move 06 to wtab01-tipo.
           move atv (04) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-inc-14
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to datv-disp.
           perform dsp-atividade-04.
           perform rot-keypress.
      *
       lab-inc-15.
           display tela-05.
           move 0 to atv (05).
           perform lmp-atividade-05.
           perform acc-atividade-05.
           if escape-key = 1
              perform lmp-atividade-05
              go to lab-inc-14
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-15
           end-if.
           if atv (05) = 0
              perform lmp-atividade-05
              go to lab-inc-16
           end-if.
           move 5 to sub-aux.
           perform rot-search.
           if erro = 0
              perform lmp-atividade-05
              go to lab-inc-15
           end-if.
           move 06 to wtab01-tipo.
           move atv (05) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-inc-15
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to datv-disp.
           perform dsp-atividade-05.
           perform rot-keypress.
           move spaces to datv-disp.
           perform dsp-atividade-05.
      *
       lab-inc-16.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              evaluate true
                       when atv (02) = 0
                            go to lab-inc-11
                       when atv (03) = 0
                            go to lab-inc-12
                       when atv (04) = 0
                            go to lab-inc-13
                       when atv (05) = 0
                            go to lab-inc-14
                       when other
                            go to lab-inc-15
              end-evaluate
           end-if.
           if resposta = "N"
              perform lmp-codigo thru lmp-atividade-05
              display tela-limpa-cad
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-16
              end-if
           end-if.
      *
       lab-inc-17.
           perform rot-move-at01.
           write reg-at01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQAT01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           display tela-06.
           perform rot-keypress.
           perform lmp-codigo thru lmp-atividade-05.
           display tela-02.
           go to lab-inc-01.
      *
       lab-inc-fim.
           perform rot-close-at01.
           perform rot-close-tabl.
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
                            when kbd2 = 69 or 101
                                 display tela-limpa-cad
                                 perform sec-consulta-empresa
                                 display tela-03
                            when kbd2 = 68 or 100
                                 display tela-limpa-cad
                                 perform sec-consulta-diretor
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
           move spaces to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              perform lmp-codigo
              go to lab-cns-codigo-fim
           end-if.
           move codigo to txt.
           perform rot-texto.
           move txt to codigo.
      *
           display tela-04.
           move low-values to at01-chave.
           move codigo to at01-codigo.
      *
       lab-cns-codigo-00-a.
           start arqat01 key is not less at01-chave.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or at01-codigo = codigo
              perform rot-inic-arquivo
              start arqat01 key is not less at01-chave
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if at01-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqat01 key is less at01-chave.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-at01
              go to lab-cns-codigo-fim
           end-if.
           if at01-chave = high-values
              perform rot-fim-arquivo
              start arqat01 key is not less at01-chave
              move spaces to codigo
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
                         move low-values to at01-chave
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to at01-chave
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-codigo thru lmp-atividade-05.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-atividade-05.
           display tela-limpa.
           exit.
      *
       sec-consulta-empresa section.
      *
       lab-cns-empresa-00.
           move spaces to empresa.
           perform lmp-empresa.
           perform acc-empresa.
           if escape-key = 1
              perform lmp-empresa
              go to lab-cns-empresa-fim
           end-if.
           display tela-04.
           move empresa to txt.
           perform rot-texto.
           move low-values to at01-chave-1.
           move txt to at01-empresa in at01-chave-1.
      *
       lab-cns-empresa-00-a.
           start arqat01 key is not less at01-chave-1.
           go to lab-cns-empresa-03.
      *
       lab-cns-empresa-01.
           perform rot-le-anterior.
           if erro not = 0 or at01-codigo = codigo 
              perform rot-inic-arquivo
              start arqat01 key is not less at01-chave-1
              move 1 to erro
              go to lab-cns-empresa-05
           end-if.
           if at01-chave = high-values
              go to lab-cns-empresa-01
           end-if.
           go to lab-cns-empresa-04.
      *
       lab-cns-empresa-02.
           start arqat01 key is less at01-chave-1.
      *
       lab-cns-empresa-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-at01
              go to lab-cns-empresa-fim
           end-if.
           if at01-chave = high-values
              perform rot-fim-arquivo
              start arqat01 key is not less at01-chave-1
              move 0 to codigo
              move 1 to erro
              go to lab-cns-empresa-05
           end-if.
      *
       lab-cns-empresa-04.
           perform rot-display.
      *
       lab-cns-empresa-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-empresa-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-empresa-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-empresa-03
                    when kbd-aux = 73
                         go to lab-cns-empresa-01
                    when kbd-aux = 71
                         move low-values to at01-chave-1
                         go to lab-cns-empresa-00-a
                    when kbd-aux = 79
                         move high-values to at01-chave-1
                         go to lab-cns-empresa-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-empresa-05
           end-if.
           perform lmp-codigo thru lmp-atividade-05.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-empresa-00.
      *
       lab-cns-empresa-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-atividade-05.
           display tela-limpa.
           exit.
      *
       sec-consulta-diretor section.
      *
       lab-cns-diretor-00.
           move spaces to diretor.
           perform lmp-diretor.
           perform acc-diretor.
           if escape-key = 1
              perform lmp-diretor
              go to lab-cns-diretor-fim
           end-if.
           display tela-04.
           move diretor to txt.
           perform rot-texto.
           move low-values to at01-chave-2.
           move txt to at01-diretor in at01-chave-2.
      *
       lab-cns-diretor-00-a.
           start arqat01 key is not less at01-chave-2.
           go to lab-cns-diretor-03.
      *
       lab-cns-diretor-01.
           perform rot-le-anterior.
           if erro not = 0 or at01-codigo = codigo 
              perform rot-inic-arquivo
              start arqat01 key is not less at01-chave-2
              move 1 to erro
              go to lab-cns-diretor-05
           end-if.
           if at01-chave = high-values
              go to lab-cns-diretor-01
           end-if.
           go to lab-cns-diretor-04.
      *
       lab-cns-diretor-02.
           start arqat01 key is less at01-chave-2.
      *
       lab-cns-diretor-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-at01
              go to lab-cns-diretor-fim
           end-if.
           if at01-chave = high-values
              perform rot-fim-arquivo
              start arqat01 key is not less at01-chave-2
              move 0 to codigo
              move 1 to erro
              go to lab-cns-diretor-05
           end-if.
      *
       lab-cns-diretor-04.
           perform rot-display.
      *
       lab-cns-diretor-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-diretor-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-diretor-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-diretor-03
                    when kbd-aux = 73
                         go to lab-cns-diretor-01
                    when kbd-aux = 71
                         move low-values to at01-chave-2
                         go to lab-cns-diretor-00-a
                    when kbd-aux = 79
                         move high-values to at01-chave-2
                         go to lab-cns-diretor-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-diretor-05
           end-if.
           perform lmp-codigo thru lmp-atividade-05.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-diretor-00.
      *
       lab-cns-diretor-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-atividade-05.
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
           perform rot-le-at01-lock.
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
           delete arqat01 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQAT01A.DAT - Tecle <Enter>
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
           unlock arqat01 record.
           display tela-04.
           exit.
      *
       sec-alteracao section.
      *
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
           perform rot-le-at01-lock.
           perform rot-display.
      *
       lab-alt-02.
           perform acc-empresa.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           move empresa to txt empresa-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-02
           end-if.
           move txt to empresa.
      *
       lab-alt-03.
           perform acc-diretor.
           if escape-key = 1
              go to lab-alt-02
           end-if.
           move diretor to txt diretor-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-03
           end-if.
           move txt to diretor.
      *
       lab-alt-04.
           perform acc-endereco.
           if escape-key = 1
              go to lab-alt-03
           end-if.
           if endereco = spaces 
              go to lab-alt-04
           end-if.
           move uf to rotina-uf.
      *
       lab-alt-05.
           display tela-08.
           move rotina-uf to uf.
           perform dsp-uf.
           perform acc-uf.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-04
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-alt-05
           end-if.           
           move uf to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-05
           end-if.
           move txt to uf.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move uf to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado
              go to lab-alt-05
           end-if.
           display tela-limpa-cad.
      *
       lab-alt-06.
           perform acc-cidade.
           if escape-key = 1
              go to lab-alt-05
           end-if.
           if cidade = spaces
              go to lab-alt-06
           end-if.
      *
       lab-alt-07.
           perform acc-cep.
           if escape-key = 1
              go to lab-alt-06
           end-if.
           if cep = 0
              go to lab-alt-07
           end-if.
      *
       lab-alt-08.
           perform acc-telefone.
           if escape-key = 1
              go to lab-alt-07
           end-if.
           if telefone = spaces
              go to lab-alt-08
           end-if.
      *
       lab-alt-08-01.
           perform acc-ddd.
           if escape-key = 1
              go to lab-alt-08
           end-if.
           if ddd = 0
              go to lab-alt-08-01
           end-if.
      *
       lab-alt-09.
           perform acc-fax.
           if escape-key = 1
              go to lab-alt-08-01
           end-if.
      *
       lab-alt-10.
           perform acc-telex.
           if escape-key = 1
              go to lab-alt-09
           end-if.
      *
       lab-alt-11.
           display tela-05.
           perform acc-atividade-01.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-10
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-11
           end-if.
           if atv (01) = 0
              go to lab-alt-11
           end-if.
           move 1 to sub-aux.
           perform rot-search.
           if erro = 0
              go to lab-alt-11
           end-if.
           move 06 to wtab01-tipo.
           move atv (01) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-alt-11
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to datv-disp.
           perform dsp-atividade-01.
           perform rot-keypress.
           move spaces to datv-disp.
           perform dsp-atividade-01.
      *
       lab-alt-12.
           display tela-05.
           perform acc-atividade-02.
           if escape-key = 1
              go to lab-alt-11
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-12
           end-if.
           if atv (02) = 0
              move 0 to atv (03) atv (04) atv (05)
              perform lmp-atividade-02 thru lmp-atividade-05
              go to lab-alt-16
           end-if.
           move 2 to sub-aux.
           perform rot-search.
           if erro = 0
              go to lab-alt-12
           end-if.
           move 06 to wtab01-tipo.
           move atv (02) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-alt-12
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to datv-disp.
           perform dsp-atividade-02.
           perform rot-keypress.
           move spaces to datv-disp.
           perform dsp-atividade-02.
      *
       lab-alt-13.
           display tela-05.
           perform acc-atividade-03.
           if escape-key = 1
              go to lab-alt-12
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-13
           end-if.
           if atv (03) = 0
              move 0 to atv (04) atv (05)
              perform lmp-atividade-03 thru lmp-atividade-05
              go to lab-alt-16
           end-if.
           move 3 to sub-aux.
           perform rot-search.
           if erro = 0
              go to lab-alt-13
           end-if.
           move 06 to wtab01-tipo.
           move atv (03) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-alt-13
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to datv-disp.
           perform dsp-atividade-03.
           perform rot-keypress.
           move spaces to datv-disp.
           perform dsp-atividade-03.
      *
       lab-alt-14.
           display tela-05.
           perform acc-atividade-04.
           if escape-key = 1
              go to lab-alt-13
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-14
           end-if.
           if atv (04) = 0
              move 0 to atv (05)
              perform lmp-atividade-05
              go to lab-alt-16
           end-if.
           move 4 to sub-aux.
           perform rot-search.
           if erro = 0
              go to lab-alt-14
           end-if.
           move 06 to wtab01-tipo.
           move atv (04) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-alt-14
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to datv-disp.
           perform dsp-atividade-04.
           perform rot-keypress.
           move spaces to datv-disp.
           perform dsp-atividade-04.
      *
       lab-alt-15.
           display tela-05.
           perform acc-atividade-05.
           if escape-key = 1
              go to lab-alt-14
           end-if.
           if escape-key = 3
              move 6 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-15
           end-if.
           if atv (05) = 0
              move spaces to datv-disp
              perform dsp-atividade-05
              go to lab-alt-16
           end-if.
           move 5 to sub-aux.
           perform rot-search.
           if erro = 0
              go to lab-alt-15
           end-if.
           move 06 to wtab01-tipo.
           move atv (05) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-atividade
              go to lab-alt-15
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to datv-disp.
           perform dsp-atividade-05.
           perform rot-keypress.
           move spaces to datv-disp.
           perform dsp-atividade-05.
      *
       lab-alt-16.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              evaluate true
                       when atv (02) = 0
                            go to lab-alt-11
                       when atv (03) = 0
                            go to lab-alt-12
                       when atv (04) = 0
                            go to lab-alt-13
                       when atv (05) = 0
                            go to lab-alt-14
                       when other
                            go to lab-alt-15
              end-evaluate
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-16
              end-if
           end-if.
           perform rot-move-at01.
           rewrite reg-at01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQAT01A.DAT - Tecle <Ent
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
           unlock arqat01 record.
           display tela-04.
           exit.