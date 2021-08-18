      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGAG01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao de Agenda :                                     *
      *                                                             *
      *  Data da ultima alteracao:    08/06/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgag01.
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
       data division.
       file section.
      *    
       copy fdag01.lib.
      *
       copy fdtabl.lib.
      *
       copy fdobs01.lib.
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
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAG01".
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
          02 codigo                    pic 9(05) value 0.
          02 assunto                   pic x(40) value spaces.
          02 assunto-a                 pic x(40) value spaces.
          02 empresa                   pic x(40) value spaces.
          02 empresa-a                 pic x(40) value spaces.
          02 nome                      pic x(40) value spaces.
          02 nome-a                    pic x(40) value spaces.
          02 partido                   pic x(20) value spaces.
          02 partido-a                 pic x(20) value spaces.
          02 data-a                    pic 9(06) value 0.
          02 data-a-disp               pic x(08) value spaces.
          02 endereco                  pic x(40) value spaces.
          02 cep                       pic 9(08) value 0.
          02 cidade                    pic x(15) value spaces.
          02 uf                        pic x(02) value spaces.
          02 ddd                       pic 9(04) value 0.
          02 telefone                  pic x(08) value spaces.
          02 telex                     pic x(08) value spaces.
          02 fax                       pic x(08) value spaces.
          02 posicao                   pic x(01) value spaces.
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
       01 campo-rotina.
          02 obs-arquivo               pic 9(02) value 0.
          02 obs-codigo                pic x(10) value spaces.
      *
       01 cab-usr.
          02 filler                    pic x(10) value "Usuario.:".
          02 cab-usuario               pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Data....:".
          02 cab-data                  pic x(08) value spaces.
      *
       01 campo-string.
          02 string-a                  pic x(40) value spaces.
          02 filler                    pic x(01) value low-values.
      *
       01 campo-lixo.
         02 lixo                       pic x(1024) value spaces.
         02 filler                     pic x(01) value low-values. 
      *
      *
       01 buffer1.
          02 filler                    pic 9(04) occurs 500.
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
          02 line 07 column 06 foreground-color 06 background-color 01
             highlight value "Codigo....:".
          02 line 08 column 06 foreground-color 06 background-color 01
             highlight value "Assunto...:".
          02 line 09 column 06 foreground-color 06 background-color 01
             highlight value "Empresa...:".
          02 line 10 column 06 foreground-color 06 background-color 01
             highlight value "Nome......:".
          02 line 11 column 06 foreground-color 06 background-color 01
             highlight value "Partido...:".
          02 line 11 column 40 foreground-color 06 background-color 01
             highlight value "Data......:".
          02 line 13 column 06 foreground-color 06 background-color 01
              highlight value "Endereco..:".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "U.F.......:".
          02 line 14 column 30 foreground-color 06 background-color 01
             highlight value "Cidade....:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "C.E.P.....:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Telefone..:".
          02 line 16 column 30 foreground-color 06 background-color 01
             highlight value "D.D.D.....:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "Fax.......:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "Telex.....:".
          02 line 19 column 06 foreground-color 06 background-color 01
             highlight value "Posicao...:".
      *
       01 tela-02.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 21 column 07 foreground-color 05 background-color 03
             value "-Help".
          02 line 21 column 15 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 17 foreground-color 05 background-color 03
             value "-Consultas".
          02 line 21 column 31 foreground-color 02 background-color 03
             highlight value "F3".
          02 line 21 column 33 foreground-color 05 background-color 03
             value "-Localizar".
      *
       01 tela-03.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 21 column 16 foreground-color 02 background-color 03
             highlight value "C".
          02 line 21 column 17 foreground-color 05 background-color 03
             value "odigo".
          02 line 21 column 26 foreground-color 02 background-color 03
             highlight value "A".
          02 line 21 column 27 foreground-color 05 background-color 03
             value "ssunto".
         02 line 21 column 37 foreground-color 02 background-color 03
             highlight value "E".
          02 line 21 column 38 foreground-color 05 background-color 03
             value "mpresa".
         02 line 21 column 48 foreground-color 02 background-color 03
             highlight value "N".
          02 line 21 column 49 foreground-color 05 background-color 03
             value "ome".
         02 line 21 column 56 foreground-color 02 background-color 03
             highlight value "P".
          02 line 21 column 57 foreground-color 05 background-color 03
             value "artido".
         02 line 21 column 66 foreground-color 02 background-color 03
             highlight value "D".
          02 line 21 column 67 foreground-color 05 background-color 03
             value "ata".
      *
       01 tela-04.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Alt".
          02 line 21 column 14 foreground-color 02 background-color 03 
             highlight value "F3".
          02 line 21 column 16 foreground-color 05 background-color 03
             value "-Exc".
          02 line 21 column 21 foreground-color 02 background-color 03 
             highlight value "F4".
          02 line 21 column 23 foreground-color 05 background-color 03
             value "-Obs".
          02 line 21 column 29 foreground-color 02 background-color 03
             highlight value "Home".
          02 line 21 column 33 foreground-color 05 background-color 03
             value "-Inic".
          02 line 21 column 40 foreground-color 02 background-color 03
             highlight value "End".
          02 line 21 column 43 foreground-color 05 background-color 03
             value "-Fim".
          02 line 21 column 50 foreground-color 02 background-color 03
             highlight value "PgDown".
          02 line 21 column 56 foreground-color 05 background-color 03
             value "-Prox".
          02 line 21 column 63 foreground-color 02 background-color 03
             highlight value "PgUp".
          02 line 21 column 67 foreground-color 05 background-color 03
             value "-Ant".
      *
       01 tela-06.
          02 line 21 column 05 foreground-color 00 background-color 01
             highlight pic x(68) from spaces.
          02 line 21 column 05 foreground-color 07 background-color 01
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-08.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 07 foreground-color 05 background-color 03
             value " - Estados".
      *
       01 tela-09.
          02 line 06 column 65 foreground-color 06 background-color 01
             highlight value "Inclusao".
      *
       01 tela-10.
          02 line 06 column 65 foreground-color 06 background-color 01
             highlight value "Consulta".
      *
       01 tela-11.
          02 line 11 column 52 foreground-color 06 background-color 01
             highlight value "Localizar".
          02 line 13 column 12 foreground-color 06 background-color 01
              highlight value "Texto.:".
      *
       01 tela-12.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 21 column 07 foreground-color 02 background-color 03 
             highlight value "S".
          02 line 21 column 08 foreground-color 05 background-color 03
             value " - Sim".
          02 line 21 column 18 foreground-color 02 background-color 03 
             highlight value "N".
          02 line 21 column 19 foreground-color 05 background-color 03
             value " - Nao".
      *
       01 tela-mensagem-cad.
          02 line 21 column 05 foreground-color 07 background-color 01
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 21 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 21 column 05 foreground-color 01 background-color 01
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
           move 21 to box-lin-f.
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
       rot-move-ag01.
           move codigo to ag01-codigo.
           move assunto to ag01-assunto.
           move assunto-a to ag01-assunto-a.
           move empresa to ag01-empresa.
           move empresa-a to ag01-empresa-a.
           move nome to ag01-nome.
           move nome-a to ag01-nome-a.
           move partido to ag01-partido.
           move partido-a to ag01-partido-a.
           move data-a to ag01-data-a.
           move endereco to ag01-endereco.
           move cep to ag01-cep.
           move cidade to ag01-cidade.
           move uf to ag01-uf.
           move ddd to ag01-ddd.
           move telefone to ag01-telefone.
           move telex to ag01-telex.
           move fax to ag01-fax.
           move posicao to ag01-posicao.
           move param-usr to ag01-usuario.
           move param-data to ag01-data.
      *
       rot-move-campos.
           move ag01-codigo to codigo.
           move ag01-assunto-a to assunto.
           move ag01-empresa-a to empresa.
           move ag01-nome-a to nome.
           move ag01-partido-a to partido.
           move ag01-data-a to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to data-a-disp.
           move dia-euro to dia-aux
           move mes-euro to mes-aux
           move ano-euro to ano-aux
           move data-aux to data-a
           move ag01-endereco to endereco.
           move ag01-uf to uf.
           move ag01-cidade to cidade.
           move ag01-cep to cep.
           move ag01-telefone to telefone.
           move ag01-ddd to ddd.
           move ag01-fax to fax.
           move ag01-telex to telex.
           move ag01-posicao to posicao.           
           move ag01-data to dias-corr.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move ag01-usuario to cab-usuario.
      *
       rot-le-ag01.
           move 0 to erro.
           read arqag01 invalid key move 1 to erro.
           if ag01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ag01.
      *
       rot-le-ag01-1.
           move 0 to erro.
           read arqag01 key ag01-chave-1 invalid key move 1 to erro.
           if ag01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ag01-1.
      *
       rot-le-ag01-2.
           move 0 to erro.
           read arqag01 key ag01-chave-2 invalid key move 1 to erro.
           if ag01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ag01-2.
      *
       rot-le-ag01-3.
           move 0 to erro.
           read arqag01 key ag01-chave-3 invalid key move 1 to erro.
           if ag01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ag01-3.
      *
       rot-le-ag01-4.
           move 0 to erro.
           read arqag01 key ag01-chave-4 invalid key move 1 to erro.
           if ag01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ag01-4.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqag01 key is equal ag01-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-ag01
           end-start.
      *
       rot-le-ag01-lock.
           move 0 to erro.
           read arqag01 next. 
           if ag01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ag01-lock
           end-if.
           read arqag01 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqag01 previous at end move 1 to erro.
           if ag01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
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
                 move zeros to reg-ag01
                 move high-values to ag01-controle
                 move 0 to ag01-numero
                 write reg-ag01-1
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
                 move zeros to reg-obs01
                 move high-values to obs01-controle
                 write reg-obs01-1
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
       rot-inic-arquivo.
           perform lmp-codigo thru lmp-posicao.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-codigo thru lmp-posicao.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-obs.
           perform rot-close-obs01.
           move ag01-codigo to obs-codigo.
           move 01 to obs-arquivo.
           call "rotobs01" using param-menu campo-rotina.
           cancel "rotobs01".
           perform rot-open-obs01.
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
           perform dsp-codigo thru dsp-posicao.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-save-buffer-01.
           call "C_Savescr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer1.
      *
       rot-rest-buffer-01.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer1.
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
       err-codigo-c.
           move " Codigo ja cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-estado.
            move " Estado nao cadastrado - Tecle <Enter>" to mensagem.
            display tela-erro-cad.
            perform rot-keypress.
            display tela-08.
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
       acc-codigo.
           accept codigo at 0718 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-assunto.
           accept assunto at 0818 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-empresa.
           accept empresa at 0918 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-nome.
           accept nome at 1018 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-partido.
           accept partido at 1118 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-a.
           accept data-a at 1152 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-endereco.
           accept endereco at 1318 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept uf at 1418 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cidade.
           accept cidade at 1442 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cep.
           accept cep at 1518 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-telefone.
           accept telefone at 1618 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-ddd.
           accept ddd at 1642 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-fax.
           accept fax at 1718 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-telex.
           accept telex at 1818 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-posicao.
           accept posicao at 1918 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-string-a.
           accept string-a at 1320 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo.
           display codigo at 0718 with foreground-color 15 
                   background-color 01.
      *
       dsp-assunto.
           display assunto at 0818 with foreground-color 15 
                   background-color 01.
      *
       dsp-empresa.
           display empresa at 0918 with foreground-color 15 
                   background-color 01.
      *
       dsp-nome.
           display nome at 1018 with foreground-color 15 
                   background-color 01.
      *
       dsp-partido.
           display partido at 1118 with foreground-color 15 
                   background-color 01.
      *
       dsp-data-a.
           display data-a-disp at 1152 with foreground-color 15 
                   background-color 01.
      *
       dsp-endereco.
           display endereco at 1318 with foreground-color 15 
                   background-color 01.
      *
       dsp-uf.
           display uf at 1418 with foreground-color 15 
                   background-color 01.
      *
       dsp-cidade.
           display cidade at 1442 with foreground-color 15 
                   background-color 01.
      *
       dsp-cep.
           display cep at 1518 with foreground-color 15 
                   background-color 01.
      *
       dsp-telefone.
           display telefone at 1618 with foreground-color 15 
                   background-color 01.
      *
       dsp-ddd.
           display ddd at 1642 with foreground-color 15 
                   background-color 01.
      *
       dsp-fax.
           display fax at 1718 with foreground-color 15 
                   background-color 01.
      *
       dsp-telex.
           display telex at 1818 with foreground-color 15 
                   background-color 01.
      *
       dsp-posicao.
           display posicao at 1918 with foreground-color 15 
                   background-color 01.
      *
       dsp-string-a.
           display string-a at 1320 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-codigo.
           display limpa at 0718 with foreground-color 15 
                   background-color 01.
      *
       lmp-assunto.
           display limpa at 0818 with foreground-color 15 
                   background-color 01.
      *
       lmp-empresa.
           display limpa at 0918 with foreground-color 15 
                   background-color 01.
      *
       lmp-nome.
           display limpa at 1018 with foreground-color 15 
                   background-color 01.
      *
       lmp-partido.
           display limpa-20 at 1118 with foreground-color 15 
                   background-color 01.
      *
       lmp-data-a.
           display limpa-10 at 1152 with foreground-color 15 
                   background-color 01.
      *
       lmp-endereco.
           display limpa at 1318 with foreground-color 15 
                   background-color 01.
      *
       lmp-uf.
           display limpa-03 at 1418 with foreground-color 15 
                   background-color 01.
      *
       lmp-cidade.
           display limpa-20 at 1442 with foreground-color 15 
                   background-color 01.
      *
       lmp-cep.
           display limpa at 1518 with foreground-color 15 
                   background-color 01.
      *
       lmp-telefone.
           display limpa-10 at 1618 with foreground-color 15 
                   background-color 01.
      *
       lmp-ddd.
           display limpa-10 at 1642 with foreground-color 15 
                   background-color 01.
      *
       lmp-fax.
           display limpa-10 at 1718 with foreground-color 15 
                   background-color 01.
      *
       lmp-telex.
           display limpa at 1818 with foreground-color 15 
                   background-color 01.
      *
       lmp-posicao.
           display limpa-10 at 1918 with foreground-color 15 
                   background-color 01.
      *
       lmp-string-a.
           display string-a at 1320 with foreground-color 15 
                   background-color 01.
      *
       sec-inclusao section.
      *
       lab-inc-00.
           display tela-limpa-cad.
           perform rot-open-tabl
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-ag01.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           display tela-09.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
      *
       lab-inc-01-00.
           display tela-09.
           display tela-02.
           initialize campos.
           move spaces to rotina-uf.
           perform lmp-codigo thru lmp-posicao.
      *
       lab-inc-01.
           perform acc-assunto.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform sec-consulta
              go to lab-inc-01-00
           end-if.
           if escape-key = 4
              perform sec-localizar
              go to lab-inc-01-00
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           move assunto to txt assunto-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-01
           end-if.       
           move txt to assunto.
      *
       lab-inc-02.
           perform acc-empresa.
           if escape-key = 1
              move assunto-a to assunto
              go to lab-inc-01
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           move empresa to txt empresa-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-02
           end-if.
           move txt to empresa.
      *
       lab-inc-03.
           perform acc-nome.
           if escape-key = 1
              move empresa-a to empresa
              go to lab-inc-02
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           move nome to txt nome-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-03
           end-if.
           move txt to nome.
      *
       lab-inc-03-01.
           perform acc-partido.
           if escape-key = 1
              move nome-a to nome
              go to lab-inc-03
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           move partido to txt partido-a.
           perform rot-texto.
           move txt to partido.
      *
       lab-inc-03-02.
           perform lmp-data-a.
           perform acc-data-a.
           if escape-key = 1
              move partido-a to partido
              go to lab-inc-03-01
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           if data-a = 0
              go to lab-inc-03-02
           end-if.
           move data-a to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-inc-03-02
           end-if.
           move data-disp to data-a-disp.
           move dias-corr to data-a
           perform dsp-data-a.

      *
       lab-inc-04.
           perform acc-endereco.
           if escape-key = 1
              perform lmp-data-a
              move data-a to dias-corr
              move 1 to opcao-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-a
              go to lab-inc-03-02
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           if endereco = spaces 
              go to lab-inc-04
           end-if.
      *
       lab-inc-05.
           display tela-08.
           move rotina-uf to uf.
           perform dsp-uf.
           perform acc-uf.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-04
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-inc-05
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
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
           perform acc-cidade.
           if escape-key = 1
              go to lab-inc-05
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           if cidade = spaces
              go to lab-inc-06
           end-if.
      *
       lab-inc-07.
           perform acc-cep.
           if escape-key = 1
              go to lab-inc-06
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           if cep = 0
              go to lab-inc-07
           end-if.
      *
       lab-inc-08.
           perform acc-telefone.
           if escape-key = 1
              go to lab-inc-07
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           if telefone = spaces
              go to lab-inc-08
           end-if.
      *
       lab-inc-08-01.
           move wtab03-ddd to ddd.
           perform acc-ddd.
           if escape-key = 1
              go to lab-inc-08
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           if ddd = 0
              go to lab-inc-08-01
           end-if.
      *
       lab-inc-09.
           perform acc-fax.
           if escape-key = 1
              go to lab-inc-08-01
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
      *
       lab-inc-10.
           perform acc-telex.
           if escape-key = 1
              go to lab-inc-09
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
      *
       lab-inc-11.
           display tela-12.
           perform acc-posicao.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-10
           end-if.
           if escape-key = 5
              go to lab-inc-01-00
           end-if.
           move posicao to txt.
           perform rot-texto.
           if txt not = "S" and "N"
              go to lab-inc-11
           end-if.
           move txt to posicao.
           perform dsp-posicao.
      *
       lab-inc-12.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-11
           end-if.
           if resposta = "N"
              move assunto-a to assunto
              move empresa-a to empresa
              move nome-a to nome
              move partido-a to partido
              move data-a to dias-corr
              move 1 to opcao-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-a
              display tela-limpa-cad
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-12
              end-if
           end-if.
      *
       lab-inc-13.
           move high-values to ag01-chave-controle.
           perform rot-ponteiro.
           perform rot-le-ag01-lock.
           if erro not = 0
              move " Erro no registro de controle - ARQAG01A.DAT - Tecle
      -       " <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              go to lab-inc-fim
           end-if.
           move ag01-numero to codigo.
           add 1 to ag01-numero codigo.
           rewrite reg-ag01.
           unlock arqag01 record.
           perform rot-move-ag01.
           write reg-ag01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQAG01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           perform dsp-codigo.
           display tela-06.
           perform rot-obs.
           perform rot-keypress.
           perform lmp-codigo thru lmp-posicao.
           display tela-02.
           go to lab-inc-01-00.
      *
       lab-inc-fim.
           perform rot-close-ag01.
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
                            when kbd2 = 65 or 97
                                 display tela-limpa-cad
                                 perform sec-consulta-assunto
                                 display tela-03
                            when kbd2 = 69 or 101
                                 display tela-limpa-cad
                                 perform sec-consulta-empresa
                                 display tela-03
                            when kbd2 = 78 or 110
                                 display tela-limpa-cad
                                 perform sec-consulta-nome
                                 display tela-03
                            when kbd2 = 80 or 112
                                 display tela-limpa-cad
                                 perform sec-consulta-partido
                                 display tela-03
                            when kbd2 = 68 or 100
                                 display tela-limpa-cad
                                 perform sec-consulta-data-a
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
      *
           display tela-04.
           move low-values to ag01-chave.
           move codigo to ag01-codigo.
      *
       lab-cns-codigo-00-a.
           start arqag01 key is not less ag01-chave.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or ag01-codigo = codigo
              perform rot-inic-arquivo
              start arqag01 key is not less ag01-chave
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if ag01-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqag01 key is less ag01-chave.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ag01
              go to lab-cns-codigo-fim
           end-if.
           if ag01-chave = high-values
              perform rot-fim-arquivo
              start arqag01 key is not less ag01-chave
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
                    when kbd-aux = 62
                         if erro = 0
                            perform rot-obs
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-codigo-03
                    when kbd-aux = 73
                         go to lab-cns-codigo-01
                    when kbd-aux = 71
                         move low-values to ag01-chave
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to ag01-chave
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa.
           exit.
      *
       sec-consulta-assunto section.
      *
       lab-cns-assunto-00.
           move spaces to assunto.
           perform lmp-assunto.
           perform acc-assunto.
           if escape-key = 1
              perform lmp-assunto
              go to lab-cns-assunto-fim
           end-if.
           display tela-04.
           move assunto to txt.
           perform rot-texto.
           move low-values to ag01-chave-1.
           move txt to ag01-assunto in ag01-chave-1.
      *
       lab-cns-assunto-00-a.
           start arqag01 key is not less ag01-chave-1.
           go to lab-cns-assunto-03.
      *
       lab-cns-assunto-01.
           perform rot-le-anterior.
           if erro not = 0 or ag01-codigo = codigo 
              perform rot-inic-arquivo
              start arqag01 key is not less ag01-chave-1
              move 1 to erro
              go to lab-cns-assunto-05
           end-if.
           if ag01-chave = high-values
              go to lab-cns-assunto-01
           end-if.
           go to lab-cns-assunto-04.
      *
       lab-cns-assunto-02.
           start arqag01 key is less ag01-chave-1.
      *
       lab-cns-assunto-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ag01
              go to lab-cns-assunto-fim
           end-if.
           if ag01-chave = high-values
              perform rot-fim-arquivo
              start arqag01 key is not less ag01-chave-1
              move 0 to codigo
              move 1 to erro
              go to lab-cns-assunto-05
           end-if.
      *
       lab-cns-assunto-04.
           perform rot-display.
      *
       lab-cns-assunto-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-assunto-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-assunto-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform rot-obs
                            go to lab-cns-assunto-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-assunto-03
                    when kbd-aux = 73
                         go to lab-cns-assunto-01
                    when kbd-aux = 71
                         move low-values to ag01-chave-1
                         go to lab-cns-assunto-00-a
                    when kbd-aux = 79
                         move high-values to ag01-chave-1
                         go to lab-cns-assunto-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-assunto-05
           end-if.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-assunto-00.
      *
       lab-cns-assunto-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-posicao.
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
           move low-values to ag01-chave-2.
           move txt to ag01-empresa in ag01-chave-2.
      *
       lab-cns-empresa-00-a.
           start arqag01 key is not less ag01-chave-2.
           go to lab-cns-empresa-03.
      *
       lab-cns-empresa-01.
           perform rot-le-anterior.
           if erro not = 0 or ag01-codigo = codigo 
              perform rot-inic-arquivo
              start arqag01 key is not less ag01-chave-2
              move 1 to erro
              go to lab-cns-empresa-05
           end-if.
           if ag01-chave = high-values
              go to lab-cns-empresa-01
           end-if.
           go to lab-cns-empresa-04.
      *
       lab-cns-empresa-02.
           start arqag01 key is less ag01-chave-2.
      *
       lab-cns-empresa-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ag01
              go to lab-cns-empresa-fim
           end-if.
           if ag01-chave = high-values
              perform rot-fim-arquivo
              start arqag01 key is not less ag01-chave-2
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
                    when kbd-aux = 62
                         if erro = 0
                            perform rot-obs
                            go to lab-cns-empresa-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-empresa-03
                    when kbd-aux = 73
                         go to lab-cns-empresa-01
                    when kbd-aux = 71
                         move low-values to ag01-chave-2
                         go to lab-cns-empresa-00-a
                    when kbd-aux = 79
                         move high-values to ag01-chave-2
                         go to lab-cns-empresa-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-empresa-05
           end-if.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-empresa-00.
      *
       lab-cns-empresa-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa.
           exit.
      *
       sec-consulta-nome section.
      *
       lab-cns-nome-00.
           move spaces to nome.
           perform lmp-nome.
           perform acc-nome.
           if escape-key = 1
              perform lmp-nome
              go to lab-cns-nome-fim
           end-if.
           display tela-04.
           move nome to txt.
           perform rot-texto.
           move low-values to ag01-chave-3.
           move txt to ag01-nome in ag01-chave-3.
      *
       lab-cns-nome-00-a.
           start arqag01 key is not less ag01-chave-3.
           go to lab-cns-nome-03.
      *
       lab-cns-nome-01.
           perform rot-le-anterior.
           if erro not = 0 or ag01-codigo = codigo 
              perform rot-inic-arquivo
              start arqag01 key is not less ag01-chave-3
              move 1 to erro
              go to lab-cns-nome-05
           end-if.
           if ag01-chave = high-values
              go to lab-cns-nome-01
           end-if.
           go to lab-cns-nome-04.
      *
       lab-cns-nome-02.
           start arqag01 key is less ag01-chave-3.
      *
       lab-cns-nome-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ag01
              go to lab-cns-nome-fim
           end-if.
           if ag01-chave = high-values
              perform rot-fim-arquivo
              start arqag01 key is not less ag01-chave-3
              move 0 to codigo
              move 1 to erro
              go to lab-cns-nome-05
           end-if.
      *
       lab-cns-nome-04.
           perform rot-display.
      *
       lab-cns-nome-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-nome-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-nome-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform rot-obs
                            go to lab-cns-nome-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-nome-03
                    when kbd-aux = 73
                         go to lab-cns-nome-01
                    when kbd-aux = 71
                         move low-values to ag01-chave-3
                         go to lab-cns-nome-00-a
                    when kbd-aux = 79
                         move high-values to ag01-chave-3
                         go to lab-cns-nome-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-nome-05
           end-if.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-nome-00.
      *
       lab-cns-nome-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa.
           exit.
      *
       sec-consulta-partido section.
      *
       lab-cns-partido-00.
           move spaces to partido.
           perform lmp-partido.
           perform acc-partido.
           if escape-key = 1
              perform lmp-partido
              go to lab-cns-partido-fim
           end-if.
           display tela-04.
           move partido to txt.
           perform rot-texto.
           move low-values to ag01-chave-4.
           move txt to ag01-partido in ag01-chave-4.
      *
       lab-cns-partido-00-a.
           start arqag01 key is not less ag01-chave-4.
           go to lab-cns-partido-03.
      *
       lab-cns-partido-01.
           perform rot-le-anterior.
           if erro not = 0 or ag01-codigo = codigo 
              perform rot-inic-arquivo
              start arqag01 key is not less ag01-chave-4
              move 1 to erro
              go to lab-cns-partido-05
           end-if.
           if ag01-chave = high-values
              go to lab-cns-partido-01
           end-if.
           go to lab-cns-partido-04.
      *
       lab-cns-partido-02.
           start arqag01 key is less ag01-chave-4.
      *
       lab-cns-partido-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ag01
              go to lab-cns-partido-fim
           end-if.
           if ag01-chave = high-values
              perform rot-fim-arquivo
              start arqag01 key is not less ag01-chave-4
              move 0 to codigo
              move 1 to erro
              go to lab-cns-partido-05
           end-if.
      *
       lab-cns-partido-04.
           perform rot-display.
      *
       lab-cns-partido-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-partido-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-partido-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform rot-obs
                            go to lab-cns-partido-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-partido-03
                    when kbd-aux = 73
                         go to lab-cns-partido-01
                    when kbd-aux = 71
                         move low-values to ag01-chave-4
                         go to lab-cns-partido-00-a
                    when kbd-aux = 79
                         move high-values to ag01-chave-4
                         go to lab-cns-partido-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-partido-05
           end-if.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-partido-00.
      *
       lab-cns-partido-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa.
           exit.
      *
       sec-consulta-data-a section.
      *
       lab-cns-data-a-00.
           move 0 to data-a.
           perform lmp-data-a.
           perform acc-data-a.
           if escape-key = 1
              perform lmp-data-a
              go to lab-cns-data-a-fim
           end-if.
           display tela-04.
           move data-a to data-aux dias-corr.
           if data-a not =  0
              move dia-aux to dia-euro
              move mes-aux to mes-euro
              move ano-aux to ano-euro
              move 4 to opcao-data
              perform rot-data
              if return-code not = 0
                 perform err-data-i
                 go to lab-cns-data-a-00
              end-if
           end-if.
           move low-values to ag01-chave-5.
           move dias-corr to ag01-data-a in ag01-chave-5.
      *
       lab-cns-data-a-00-a.
           start arqag01 key is not less ag01-chave-5.
           go to lab-cns-data-a-03.
      *
       lab-cns-data-a-01.
           perform rot-le-anterior.
           if erro not = 0 or ag01-codigo = codigo 
              perform rot-inic-arquivo
              start arqag01 key is not less ag01-chave-5
              move 1 to erro
              go to lab-cns-data-a-05
           end-if.
           if ag01-chave = high-values
              go to lab-cns-data-a-01
           end-if.
           go to lab-cns-data-a-04.
      *
       lab-cns-data-a-02.
           start arqag01 key is less ag01-chave-5.
      *
       lab-cns-data-a-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ag01
              go to lab-cns-data-a-fim
           end-if.
           if ag01-chave = high-values
              perform rot-fim-arquivo
              start arqag01 key is not less ag01-chave-5
              move 0 to codigo
              move 1 to erro
              go to lab-cns-data-a-05
           end-if.
      *
       lab-cns-data-a-04.
           perform rot-display.
      *
       lab-cns-data-a-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-data-a-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-data-a-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform rot-obs
                            go to lab-cns-data-a-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-data-a-03
                    when kbd-aux = 73
                         go to lab-cns-data-a-01
                    when kbd-aux = 71
                         move low-values to ag01-chave-5
                         go to lab-cns-data-a-00-a
                    when kbd-aux = 79
                         move high-values to ag01-chave-5
                         go to lab-cns-data-a-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-data-a-05
           end-if.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-data-a-00.
      *
       lab-cns-data-a-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa.
           exit.
      *
       sec-exclusao section.
      *
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
           perform rot-le-ag01-lock.
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
           delete arqag01 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQAG01A.DAT - Tecle <Enter>
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
           unlock arqag01 record.
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
           perform rot-le-ag01-lock.
           perform rot-display.
           move uf to rotina-uf.
      *
       lab-alt-01.
           perform acc-assunto.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           move assunto to txt assunto-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-01
           end-if.       
           move txt to assunto.
      *
       lab-alt-02.
           perform acc-empresa.
           if escape-key = 1
              move assunto-a to assunto
              go to lab-alt-01
           end-if.
           move empresa to txt empresa-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-02
           end-if.
           move txt to empresa.
      *
       lab-alt-03.
           perform acc-nome.
           if escape-key = 1
              move empresa-a to empresa
              go to lab-alt-02
           end-if.
           move nome to txt nome-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-03
           end-if.
           move txt to nome.
      *
       lab-alt-03-01.
           perform acc-partido.
           if escape-key = 1
              move nome-a to nome
              go to lab-alt-03
           end-if.
           move partido to txt partido-a.
           perform rot-texto.
           move txt to partido.
      *
       lab-alt-03-02.
           perform lmp-data-a.
           perform acc-data-a.
           if escape-key = 1
              move partido-a to partido
              go to lab-alt-03-01
           end-if.
           if data-a = 0
              go to lab-alt-03-02
           end-if.
           move data-a to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-alt-03-02
           end-if.
           move data-disp to data-a-disp.
           move dias-corr to data-a
           perform dsp-data-a.

      *
       lab-alt-04.
           perform acc-endereco.
           if escape-key = 1
              perform lmp-data-a
              move data-a to dias-corr
              move 1 to opcao-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-a
              go to lab-alt-03-02
           end-if.
           if endereco = spaces 
              go to lab-alt-04
           end-if.
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
           move reg-tabl to reg-wtab03.
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
           display tela-12.
           perform acc-posicao.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-10
           end-if.
           move posicao to txt.
           perform rot-texto.
           if txt not = "S" and "N"
              go to lab-alt-11
           end-if.
           move txt to posicao.
           perform dsp-posicao.
      *
       lab-alt-12.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-11
           end-if.
           if resposta = "N"
              move assunto-a to assunto
              move empresa-a to empresa
              move nome-a to nome
              move partido-a to partido
              move data-a to dias-corr
              move 1 to opcao-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-a
              display tela-limpa-cad
              go to lab-alt-01
           else
              if resposta not = "S"
                 go to lab-alt-12
              end-if
           end-if.
           perform rot-move-ag01.
           rewrite reg-ag01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQAG01A.DAT - Tecle <Ent
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
           unlock arqag01 record.
           display tela-04.
           exit.
      *
       sec-localizar section.
      *
       lab-loc-00-00.
           display tela-10.
           perform rot-open-obs01.
           move 09 to box-col box-lin.
           move 62 to box-col-f.
           move 15 to box-lin-f.
           perform rot-save-buffer-01.
           move 60 to box-col-f.
           move 14 to box-lin-f.
           perform rot-box.
           display tela-11.
      *
       lab-loc-00.
           move spaces to string-a.
           perform lmp-string-a.
           perform acc-string-a.
           if escape-key = 1
              go to lab-loc-fim
           end-if.
           if escape-key = 5
              go to lab-loc-00
           end-if.
           move 09 to box-col box-lin.
           move 62 to box-col-f.
           move 15 to box-lin-f.
           perform rot-rest-buffer-01.
           display tela-04.
           call "C_Strfim" using by reference campo-string.
           call "C_strupr" using by reference campo-string.
           move low-values to ag01-chave.
      *
       lab-loc-00-a.
           start arqag01 key is not less ag01-chave.
           go to lab-loc-03.
      *
       lab-loc-01.
           perform rot-le-anterior.
           if erro not = 0 or ag01-codigo = codigo
              perform rot-inic-arquivo
              start arqag01 key is not less ag01-chave
              move 1 to erro
              go to lab-loc-05
           end-if.
           if ag01-chave = high-values
              go to lab-loc-01
           end-if.
           move reg-ag01 to lixo.
           call "C_strupr" using by reference campo-lixo.
           call "C_Strloc" using by reference campo-lixo
                                 by reference campo-string.
           if return-code = 1
              go to lab-loc-04
           end-if.
           move ag01-codigo to obs01-codigo.
           move 01 to obs01-arquivo.
           perform rot-le-obs01.
           if erro = 0
              move reg-obs01 to lixo
              call "C_strupr" using by reference campo-lixo
              call "C_Strloc" using by reference campo-lixo
                                    by reference campo-string
           end-if.
           if return-code = 0
              go to lab-loc-01
           else
              go to lab-loc-04
           end-if.

      *
       lab-loc-02.
           start arqag01 key is less ag01-chave.
      *
       lab-loc-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-ag01
              go to lab-loc-fim
           end-if.
           if ag01-chave = high-values
              perform rot-fim-arquivo
              start arqag01 key is not less ag01-chave
              move 0 to codigo
              move 1 to erro
              go to lab-loc-05
           end-if.
           move reg-ag01 to lixo.
           call "C_strupr" using by reference campo-lixo.
           call "C_Strloc" using by reference campo-lixo
                                 by reference campo-string.
           if return-code = 1
              go to lab-loc-04
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
           if return-code = 0
              go to lab-loc-03
           end-if.
      *
       lab-loc-04.
           perform rot-display.
      *
       lab-loc-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-loc-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-loc-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform rot-obs
                            go to lab-loc-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-loc-03
                    when kbd-aux = 73
                         go to lab-loc-01
                    when kbd-aux = 71
                         move low-values to ag01-chave
                         go to lab-loc-00-a
                    when kbd-aux = 79
                         move high-values to ag01-chave
                         go to lab-loc-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-loc-05
           end-if.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-loc-00-00.
      *
       lab-loc-fim.
           move zeros to campo-kbd.
           move 09 to box-col box-lin.
           move 62 to box-col-f.
           move 15 to box-lin-f.
           perform rot-rest-buffer-01.
           perform lmp-codigo thru lmp-posicao.
           display tela-limpa.
           perform rot-close-obs01
           exit.
      *