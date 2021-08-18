      ***************************************************************
      *                                                             *
      *  A B A V / S P -  I N F O R M A T I C A   :::  PGSL01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao do cadastro de participantes :                  *
      *                                                             *
      *  Data da ultima alteracao:    18/04/93     v1.00            *
      *                               23/03/94     v1.01            *
      *                               07/05/95     v1.02            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgsl01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqsl01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is sl01-chave
                  alternate record key is sl01-chave-1 with duplicates
                  alternate record key is sl01-chave-2 with duplicates
                  alternate record key is sl01-chave-3 with duplicates
                  alternate record key is sl01-chave-4 with duplicates
                  file status is sl01-status.
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
           select arqimp assign to printer
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdsl01.lib.
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
       01 sl01-status                  pic x(02) value "00".
       01 sl01-stat                    pic x(01) value "F".
      *
       01 nome-arq-sl01.
          02 sl01-dir                  pic x(03) value "SL1".
          02 filler                    pic x(01) value "\".
          02 sl01-nome                 pic x(08) value "ARQSL01A".
          02 filler                    pic x(01) value ".".
          02 sl01-ext                  pic x(03) value "DAT".
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
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGSL01".
          02 cb-versao                 pic x(06) value "v1.02 ".
      *
       01 limpa                        pic x(45) value spaces.
       01 limpa-aux                    pic x(15) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-empresa                 pic x(01) value "N".
      *
       01 campos.
          02 codigo                    pic 9(05) value 0.
          02 empresa                   pic x(40) value spaces.
          02 empresa-a                 pic x(40) value spaces.
          02 grupo                     pic x(01) value spaces.
          02 uf                        pic x(02) value spaces.
          02 nome                      pic x(40) value spaces.
          02 nome-a                    pic x(40) value spaces.
          02 nome-cracha               pic x(15) value spaces.
          02 cargo                     pic x(15) value spaces.
          02 endereco                  pic x(40) value spaces.
          02 cep                       pic 9(08) value 0.
          02 cep-disp                  pic 9(05)b9(03) value 0.
          02 cidade                    pic x(15) value spaces.
          02 ddd                       pic 9(04) value 0.
          02 telefone                  pic x(08) value spaces.
          02 telex                     pic x(08) value spaces.
          02 fax                       pic x(08) value spaces.
          02 presenca                  pic x(01) value spaces.
          02 tkt                       pic x(01) value spaces.
          02 etiqueta                  pic x(01) value spaces.
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
       01 cab-01.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "".
          02 cab-nome-c                pic x(16) value spaces.
          02 filler                    pic x(01) value "".
          02 filler                    pic x(12) value spaces.
          02 cab-empresa-e             pic x(35) value spaces.
      *
       01 cab-02.
          02 filler                    pic x(45) value spaces.
          02 cab-ac-e                  pic x(03) value "A/C".
          02 filler                    pic x(01) value spaces.
          02 cab-nome-e                pic x(31) value spaces.
      *
       01 cab-03.
          02 filler                    pic x(01) value spaces.
          02 cab-empresa-c             pic x(38) value spaces.
          02 filler                    pic x(06) value spaces.
          02 cab-endereco-e            pic x(35) value spaces.
      *
       01 cab-04.
          02 filler                    pic x(01) value spaces.
          02 cab-cidade-c              pic x(15) value spaces.
          02 filler                    pic x(05) value spaces.
          02 cab-uf-c                  pic x(02) value spaces.
          02 filler                    pic x(22) value spaces.
          02 cab-cep-e                 pic 9(05)b9(03) value 0
             blank when zero.
          02 filler                    pic x(02) value spaces.
          02 cab-cidade-e              pic x(15) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-uf-e                  pic x(02) value spaces.
      *
       01 cab-05.
          02 filler                    pic x(40) value spaces.
          02 cab-grupo-c               pic x(01) value spaces.
      *
       01 cab-usr.
          02 filler                    pic x(10) value "Usuario.:".
          02 cab-usuario               pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Data....:".
          02 cab-data                  pic x(08) value spaces.
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
          02 line 07 column 05 foreground-color 06 background-color 01 
             highlight value "Codigo....:".
          02 line 08 column 05 foreground-color 06 background-color 01 
             highlight value "Nome......:".
          02 line 09 column 05 foreground-color 06 background-color 01 
             highlight value "Cargo.....:".
          02 line 09 column 34 foreground-color 06 background-color 01 
             highlight value "Nome Cracha..:".
          02 line 10 column 05 foreground-color 06 background-color 01 
             highlight value "Empresa...:".
          02 line 12 column 05 foreground-color 06 background-color 01 
             highlight value "Endereco..:".
          02 line 13 column 05 foreground-color 06 background-color 01 
             highlight value "C.e.p.....:".
          02 line 13 column 34 foreground-color 06 background-color 01 
             highlight value "U.F..........:".
          02 line 14 column 05 foreground-color 06 background-color 01 
             highlight value "Cidade....:".
          02 line 14 column 34 foreground-color 06 background-color 01 
             highlight value "D.D.D........:".
          02 line 15 column 05 foreground-color 06 background-color 01 
             highlight value "Telefone..:".
          02 line 15 column 34 foreground-color 06 background-color 01 
             highlight value "Telex........:".
          02 line 16 column 05 foreground-color 06 background-color 01 
             highlight value "Fax.......:".
          02 line 16 column 34 foreground-color 06 background-color 01 
             highlight value "Grupo........:".
          02 line 17 column 05 foreground-color 06 background-color 01 
             highlight value "T.K.T.....:".
          02 line 17 column 34 foreground-color 06 background-color 01 
             highlight value "Presenca.....:".
      *
       01 tela-02.
          02 line 19 column 04 foreground-color 02 background-color 03
             highlight pic x(65) from spaces.
          02 line 19 column 05 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 19 column 07 foreground-color 05 background-color 03
             value "-Help".
          02 line 19 column 15 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 19 column 17 foreground-color 05 background-color 03
             value "-Consultas".
          02 line 19 column 30 foreground-color 02 background-color 03
             highlight value "F3".
          02 line 19 column 32 foreground-color 05 background-color 03
             value "-Crachas".
      *
       01 tela-03.
          02 line 19 column 04 foreground-color 02 background-color 03
             highlight pic x(65) from spaces.
          02 line 19 column 05 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 19 column 07 foreground-color 05 background-color 03
             value "-Help".
          02 line 19 column 15 foreground-color 02 background-color 03
             highlight value "C".
          02 line 19 column 16 foreground-color 05 background-color 03
             value "odigo".
          02 line 19 column 25 foreground-color 02 background-color 03
             highlight value "E".
          02 line 19 column 26 foreground-color 05 background-color 03
             value "mpresa".
          02 line 19 column 37 foreground-color 02 background-color 03
             highlight value "N".
          02 line 19 column 38 foreground-color 05 background-color 03
             value "ome".
          02 line 19 column 46 foreground-color 02 background-color 03
             highlight value "G".
          02 line 19 column 47 foreground-color 05 background-color 03
             value "rupo".
      *
       01 tela-04.
          02 line 19 column 04 foreground-color 02 background-color 03
             highlight pic x(65) from spaces.
          02 line 19 column 05 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 19 column 07 foreground-color 05 background-color 03
             value "-Alt".
          02 line 19 column 13 foreground-color 02 background-color 03
             highlight value "F3".
          02 line 19 column 15 foreground-color 05 background-color 03
             value "-Exc".
          02 line 19 column 21 foreground-color 02 background-color 03
             highlight value "F4".
          02 line 19 column 23 foreground-color 05 background-color 03
             value "-Etiq".
          02 line 19 column 30 foreground-color 02 background-color 03
             highlight value "Home".
          02 line 19 column 34 foreground-color 05 background-color 03
             value "-Inic".
          02 line 19 column 40 foreground-color 02 background-color 03
             highlight value "End".
          02 line 19 column 43 foreground-color 05 background-color 03
             value "-Fim".
          02 line 19 column 48 foreground-color 02 background-color 03
             highlight value "PgDown".
          02 line 19 column 54 foreground-color 05 background-color 03
             value "-Prox".
          02 line 19 column 60 foreground-color 02 background-color 03
             highlight value "PgUp".
          02 line 19 column 64 foreground-color 05 background-color 03
             value "-Ant".
      *
       01 tela-05.
          02 line 19 column 04 foreground-color 02 background-color 03
             highlight pic x(65) from spaces.
          02 line 19 column 05 foreground-color 02 background-color 03
             highlight value "O".
          02 line 19 column 06 foreground-color 05 background-color 03
             value " - Org".
          02 line 19 column 16 foreground-color 02 background-color 03
             highlight value "I".
          02 line 19 column 17 foreground-color 05 background-color 03
             value " - Imp".
          02 line 19 column 26 foreground-color 02 background-color 03
             highlight value "V".
          02 line 19 column 27 foreground-color 05 background-color 03
             value " - Vis".
          02 line 19 column 37 foreground-color 02 background-color 03
             highlight value "A".
          02 line 19 column 38 foreground-color 05 background-color 03
             value " - Age".
          02 line 19 column 46 foreground-color 02 background-color 03
             highlight value "F".
          02 line 19 column 47 foreground-color 05 background-color 03
             value " - Prof".
          02 line 19 column 57 foreground-color 02 background-color 03
             highlight value "M".
          02 line 19 column 58 foreground-color 05 background-color 03
             value " - Prom".
      *
       01 tela-07.
          02 line 19 column 04 foreground-color 02 background-color 03
             highlight pic x(65) from spaces.
          02 line 19 column 04 foreground-color 02 background-color 03
             highlight value "Registro gravado - Tecle <Enter>".
          02 line 19 column 38 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 19 column 40 foreground-color 02 background-color 03
             highlight value "-Etiqueta".
      *
       01 tela-08.
          02 line 19 column 04 foreground-color 02 background-color 03
             highlight pic x(65) from spaces.
          02 line 19 column 05 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 19 column 07 foreground-color 05 background-color 03
             value "-Estados".
      *
       01 tela-09.
          02 line 19 column 04 foreground-color 02 background-color 03
             highlight pic x(65) from spaces.
          02 line 19 column 05 foreground-color 02 background-color 03
             highlight value "S".
          02 line 19 column 06 foreground-color 05 background-color 03
             value " - Sim".
          02 line 19 column 18 foreground-color 02 background-color 03
             highlight value "N".
          02 line 19 column 19 foreground-color 05 background-color 03
             value " - Nao".
      *
       01 tela-10.
          02 line 06 column 61 foreground-color 06 background-color 01
             highlight value "Inclusao".
      *
       01 tela-11.
          02 line 06 column 61 foreground-color 06 background-color 01
             highlight value "Consulta".
      *
       01 tela-12.
          02 line 06 column 61 foreground-color 06 background-color 01
             highlight value "  Cracha".
      *
       01 tela-mensagem-cad.
          02 line 19 column 04 foreground-color 07 background-color 01
             highlight pic x(65) from mensagem.
      *
       01 tela-erro-cad.
          02 line 19 column 04 beep reverse-video pic x(65) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 19 column 04 foreground-color 01 background-color 01
             pic x(65) from spaces.
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
           move 02 to box-col.
           move 04 to box-lin.
           move 68 to box-col-f.
           move 19 to box-lin-f.
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
       rot-move-sl01.
           move zeros to reg-sl01.
           move codigo to sl01-codigo.
           move empresa to sl01-empresa in sl01-chave-1
                           sl01-empresa in sl01-chave-3
                           sl01-empresa in sl01-chave-4.
           move empresa-a to sl01-empresa-a.
           move nome to sl01-nome in sl01-chave-2
                        sl01-nome in sl01-chave-4.
           move nome-a to sl01-nome-a.
           move grupo to sl01-grupo.
           move nome-cracha to sl01-nome-cracha.
           move cargo to sl01-cargo.
           move endereco to sl01-endereco.
           move cep to sl01-cep.
           move uf to sl01-uf.
           move cidade to sl01-cidade.
           move ddd to sl01-ddd.
           move telefone to sl01-telefone.
           move telex to sl01-telex.
           move fax to sl01-fax.
           move presenca to sl01-presenca.
           move tkt to sl01-tkt.
           move etiqueta to sl01-etiqueta.
           move param-usr to sl01-usuario.
           move param-data to sl01-data.
      *
       rot-move-campos.
           if flag-empresa not = "S"
              move sl01-codigo to codigo
              move sl01-nome-a to nome
              move sl01-empresa-a to empresa cab-empresa-c
              move sl01-cargo to cargo
              move sl01-nome-cracha to nome-cracha cab-nome-c
              move sl01-etiqueta to etiqueta
              move sl01-usuario to cab-usuario
              move sl01-data to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to cab-data
              move sl01-presenca to presenca
           end-if.
           move sl01-grupo to grupo cab-grupo-c.
           move sl01-endereco to endereco.
           move sl01-cep to cep cep-disp.
           move sl01-uf to uf cab-uf-c.
           move sl01-cidade to cidade cab-cidade-c.
           move sl01-ddd to ddd.
           move sl01-telefone to telefone.
           move sl01-telex to telex.
           move sl01-fax to fax.
           move sl01-tkt to tkt.
      *     if sl01-grupo = "A"
              move sl01-empresa-a to cab-empresa-e
              move sl01-nome-a to cab-nome-e
              move sl01-endereco to cab-endereco-e
              move sl01-cep to cab-cep-e
              move sl01-cidade to cab-cidade-e
              move sl01-uf to cab-uf-e
              move "A/C" to cab-ac-e.
      *   else
      *        move spaces to cab-empresa-e
      *        move spaces to cab-nome-e
      *        move spaces to cab-endereco-e
      *        move 0 to cab-cep-e
      *        move spaces to cab-cidade-e
      *        move spaces to cab-uf-e
      *        move spaces to cab-ac-e
      *   end-if.
      *
       rot-le-tabl.
           move 0 to erro.
           read arqtabl invalid key move 1 to erro.
           if tabl-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-tabl.
      *
       rot-le-sl01-2.
           move 0 to erro.
           read arqsl01 key sl01-chave-2 invalid key move 1 to erro.
           if sl01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-sl01-2.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqsl01 key is equal sl01-chave invalid key
                 move 1 to erro
                 perform err-leitura-sl01
           end-start.
      *
       rot-le-sl01-lock.
           move 0 to erro.
           read arqsl01 next. 
           if sl01-status = "68"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-sl01-lock
           end-if
           read arqsl01 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqsl01 previous at end move 1 to erro.
           if sl01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqsl01 next at end move 1 to erro.
           if sl01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-pesq-tabela-3.
           perform rot-close-tabl.
           move 15 to rotina-col-uf.
           move 12 to rotina-lin-uf.
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
       rot-open-sl01.
           move 0 to erro.
           if sl01-stat = "F"
              open i-o arqsl01
              if sl01-status not = "00"
                 move 
                 " Erro de abertura no ARQSL01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to sl01-stat
               end-if
           end-if.
      *
       rot-close-sl01.
           if sl01-stat = "A"
              close arqsl01
              move "F" to sl01-stat
           end-if.
      *
       err-leitura-sl01.
           move " Erro de leitura - ARQSL01A.DAT - Tecle <Enter>" to
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
       rot-inic-arquivo.
           perform lmp-codigo thru lmp-presenca.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-codigo thru lmp-presenca.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       err-estado.
            move " Estado nao cadastrado - Tecle <Enter>" to mensagem.
            display tela-erro-cad.
            perform rot-keypress.
            display tela-08.
      *
       err-nome-c.
           move " Nome ja cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-move-campos.
           perform dsp-codigo thru dsp-presenca.
           perform rot-keypress.
           perform lmp-codigo thru lmp-presenca.
           display tela-limpa-cad.
      *
       err-nome-c-alt.
           move " Nome ja cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-display.
           perform rot-move-campos.
           perform dsp-codigo thru dsp-presenca.
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
           accept resposta at 1868 with auto foreground-color 01
                                             background-color 01.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
      *  Sequencia para dar accept
      *
       acc-codigo.
           accept codigo at 0717 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-nome.
           accept nome at 0817 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cargo.
           accept cargo at 0917 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-nome-cracha.
           accept nome-cracha at 0949 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-empresa.
           accept empresa at 1017 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-endereco.
           accept endereco at 1217 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cep.
           accept cep at 1317 with auto update
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept uf at 1349 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cidade.
           accept cidade at 1417 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-ddd.
           accept ddd at 1449 with auto update
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-telefone.
           accept telefone at 1517 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-telex.
           accept telex at 1549 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-fax.
           accept fax at 1617 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-grupo.
           accept grupo at 1649 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-tkt.
           accept tkt at 1717 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-presenca.
           accept presenca at 1749 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo.
           display codigo at 0717 with foreground-color 15 
                   background-color 01.
      *
       dsp-nome.
           display nome at 0817 with foreground-color 15 
                   background-color 01.
      *
       dsp-cargo.
           display cargo at 0917 with foreground-color 15 
                   background-color 01.
      *
       dsp-nome-cracha.
           display nome-cracha at 0949 with foreground-color 15 
                   background-color 01.
      *
       dsp-empresa.
           display empresa at 1017 with foreground-color 15 
                   background-color 01.
      *
       dsp-endereco.
           display endereco at 1217 with foreground-color 15 
                   background-color 01.
      *
       dsp-cep.
           display cep-disp at 1317 with foreground-color 15 
                   background-color 01.
      *
       dsp-uf.
           display uf at 1349 with foreground-color 15 
                   background-color 01.
      *
       dsp-cidade.
           display cidade at 1417 with foreground-color 15 
                   background-color 01.
      *
       dsp-ddd.
           display ddd at 1449 with foreground-color 15 
                   background-color 01.
      *
       dsp-telefone.
           display telefone at 1517 with foreground-color 15 
                   background-color 01.
      *
       dsp-telex.
           display telex at 1549 with foreground-color 15 
                   background-color 01.
      *
       dsp-fax.
           display fax at 1617 with foreground-color 15 
                   background-color 01.
      *
       dsp-grupo.
           display grupo at 1649 with foreground-color 15 
                   background-color 01.
      *
       dsp-tkt.
           display tkt at 1717 with foreground-color 15 
                   background-color 01.
      *
       dsp-presenca.
           display presenca at 1749 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-codigo.
           display limpa-aux at 0717 with foreground-color 15 
                   background-color 01.
      *
       lmp-nome.
           display limpa at 0817 with foreground-color 15 
                   background-color 01.
      *
       lmp-cargo.
           display limpa-aux at 0917 with foreground-color 15 
                   background-color 01.
      *
       lmp-nome-cracha.
           display limpa-aux at 0949 with foreground-color 15 
                   background-color 01.
      *
       lmp-empresa.
           display limpa at 1017 with foreground-color 15 
                   background-color 01.
      *
       lmp-endereco.
           display limpa at 1217 with foreground-color 15 
                   background-color 01.
      *
       lmp-cep.
           display limpa-aux at 1317 with foreground-color 15 
                   background-color 01.
      *
       lmp-uf.
           display limpa-aux at 1349 with foreground-color 15 
                   background-color 01.
      *
       lmp-cidade.
           display limpa-aux at 1417 with foreground-color 15 
                   background-color 01.
      *
       lmp-ddd.
           display limpa-aux at 1449 with foreground-color 15 
                   background-color 01.
      *
       lmp-telefone.
           display limpa-aux at 1517 with foreground-color 15 
                   background-color 01.
      *
       lmp-telex.
           display limpa-aux at 1549 with foreground-color 15 
                   background-color 01.
      *
       lmp-fax.
           display limpa-aux at 1617 with foreground-color 15 
                   background-color 01.
      *
       lmp-grupo.
           display limpa-aux at 1649 with foreground-color 15 
                   background-color 01.
      *
       lmp-tkt.
           display limpa-aux at 1717 with foreground-color 15 
                   background-color 01.
      *
       lmp-presenca.
           display limpa-aux at 1749 with foreground-color 15 
                   background-color 01.
      *
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
       sec-inclusao section.
      *
       lab-inc-00.
           perform rot-open-sl01.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-tabl.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           display tela-limpa-cad.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
      *
       lab-inc-01.
           display tela-10.
           display tela-02.
           move "N" to flag-empresa.
           move spaces to nome.
           perform lmp-nome.
           perform acc-nome.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform lmp-nome
              display tela-limpa-cad
              perform sec-consulta
              go to lab-inc-01
           end-if.
           if escape-key = 4
              perform lmp-nome
              display tela-limpa-cad
              perform sec-inclusao-c
              go to lab-inc-01
           end-if.
           move nome to txt nome-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-01
           end-if.
           move txt to nome sl01-nome in sl01-chave-2.
           perform rot-le-sl01-2.
           if erro = 0
              perform err-nome-c
              go to lab-inc-01
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-02.
           move spaces to cargo.
           perform lmp-cargo.
           perform acc-cargo.
           if escape-key = 1
              perform lmp-cargo
              go to lab-inc-01
           end-if.
           if cargo = spaces
              go to lab-inc-02
           end-if.
      *
       lab-inc-03.
           move spaces to nome-cracha.
           perform lmp-nome-cracha.
           perform acc-nome-cracha.
           if escape-key = 1
              perform lmp-nome-cracha
              go to lab-inc-02
           end-if.
           if nome-cracha = spaces
              go to lab-inc-03
           end-if.
           move "N" to flag-empresa.
      *
       lab-inc-04.
           if flag-empresa = "N"
              move spaces to empresa
           end-if.
           perform lmp-empresa.
           perform acc-empresa.
           if escape-key = 1
              perform lmp-empresa thru lmp-presenca
              go to lab-inc-03
           end-if.
           move empresa to txt empresa-a.
           perform rot-texto.
           if txt = spaces
              if sl01-empresa-a not = spaces
                 move "S" to flag-empresa
                 perform rot-move-campos
                 move sl01-empresa-a to empresa
                 perform dsp-empresa thru dsp-tkt
                 move sl01-empresa in sl01-chave-1 to empresa
                 move sl01-empresa-a to empresa-a
                 go to lab-inc-15
              else
                 go to lab-inc-04
              end-if
           else
              move txt to sl01-empresa in sl01-chave-1 empresa
              move 0 to erro
              start arqsl01 key is equal sl01-chave-1 
                    invalid key move 1 to erro
              end-start
              if erro = 0
                 perform rot-le-proximo
                 move "S" to flag-empresa
                 perform rot-move-campos
                 perform dsp-endereco thru dsp-tkt
                 go to lab-inc-15
              end-if       
           end-if.
      *
       lab-inc-05.
           if flag-empresa = "N"
              move spaces to endereco
              perform lmp-endereco
           end-if.
           perform acc-endereco.
           if escape-key = 1
              if flag-empresa = "N"
                 perform lmp-endereco
              else
                 move empresa-a to empresa
              end-if
              go to lab-inc-04
           end-if.
           if endereco = spaces 
              go to lab-inc-05
           end-if.
      *
       lab-inc-06.
           if flag-empresa = "N"
              move 0 to cep
           end-if.
           perform lmp-cep.
           perform acc-cep.
           if escape-key = 1
              if flag-empresa = "N"
                 perform lmp-cep
              else
                 move cep to cep-disp
                 perform dsp-cep
              end-if
              go to lab-inc-05
           end-if.
           if cep = 0
              go to lab-inc-06
           end-if.
           if flag-empresa = "N"
              move spaces to rotina-uf
           else
              move uf to rotina-uf
           end-if.
           move cep to cep-disp.
           perform dsp-cep.
      *
       lab-inc-07.
           display tela-08.
           move rotina-uf to uf.
           if flag-empresa = "N"
              perform lmp-uf
           end-if.
           perform acc-uf.
           if escape-key = 1
              if flag-empresa = "N"
                 perform lmp-uf
              end-if
              display tela-limpa-cad
              go to lab-inc-06
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-inc-07
           end-if.
           move uf to txt.
           perform rot-texto.
           move txt to uf.
           if uf = spaces
              go to lab-inc-07
           end-if.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move uf to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado
              go to lab-inc-07
           end-if.
           move reg-tabl to reg-wtab03.
           display tela-limpa-cad.
           if flag-empresa = "S"
              move cidade to wtab03-capital
              move ddd to wtab03-ddd
           end-if.
      *
       lab-inc-08.
           move wtab03-capital to cidade.
           if flag-empresa = "N"
              perform lmp-cidade
           end-if.
           perform acc-cidade.
           if escape-key = 1
              if flag-empresa = "N"
                 perform lmp-cidade
              end-if
              go to lab-inc-07
           end-if.
           if cidade = spaces
              go to lab-inc-08
           end-if.
           move cidade to wtab03-capital.
      *
       lab-inc-09.
           move wtab03-ddd to ddd.
           if flag-empresa = "N"
              perform lmp-ddd
           end-if.
           perform acc-ddd.
           if escape-key = 1
              if flag-empresa = "N"
                 perform lmp-ddd
              end-if
              go to lab-inc-08
           end-if.
           move ddd to wtab03-ddd.
      *
       lab-inc-10.
           if flag-empresa = "N"
              move spaces to telefone
              perform lmp-telefone
           end-if.
           perform acc-telefone.
           if escape-key = 1
              if flag-empresa = "N"
                 perform lmp-telefone
              end-if
              go to lab-inc-09
           end-if.
      *
       lab-inc-11.
           if flag-empresa = "N"
              move spaces to telex
              perform lmp-telex
           end-if.
           perform acc-telex.
           if escape-key = 1
              if flag-empresa = "N"
                 perform lmp-telex
              end-if
              go to lab-inc-10
           end-if.
      *
       lab-inc-12.
           if flag-empresa = "N"
              move spaces to fax
              perform lmp-fax
           end-if.
           perform acc-fax.
           if escape-key = 1
              if flag-empresa = "N"
                 perform lmp-fax
              end-if
              go to lab-inc-11
           end-if.
      *
       lab-inc-13.
           display tela-05.
           if flag-empresa = "N"
              move space to grupo
              perform lmp-grupo
           end-if.
           perform acc-grupo.
           if escape-key = 1
              if flag-empresa = "N"
                 perform lmp-grupo
              end-if
              display tela-limpa-cad
              go to lab-inc-12
           end-if.
           move grupo to txt.
           perform rot-texto.
           if txt not = "O" and "I" and "F" and "V" and "M" and "A"
              go to lab-inc-13
           end-if.
           move txt to grupo.
           perform dsp-grupo.
           display tela-limpa-cad.
      *
       lab-inc-14.
           if flag-empresa = "N"
              move space to tkt
              perform lmp-tkt
           end-if.
           perform acc-tkt.
           if escape-key = 1
              if flag-empresa = "N"
                 perform lmp-tkt
              end-if
              display tela-limpa-cad
              go to lab-inc-13
           end-if.
           move tkt to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-14
           end-if.
           move txt to tkt.
           perform dsp-tkt.
      *
       lab-inc-15.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-14
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-codigo thru lmp-presenca
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-15
              end-if
           end-if.
           move high-values to sl01-chave-controle.
           perform rot-ponteiro.
           perform rot-le-sl01-lock.
           if erro not = 0
              move " Erro no registro de controle - ARQSL01A.DAT - Tecle
      -       " <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              go to lab-inc-fim
           end-if.
           move sl01-numero to codigo.
           add 1 to sl01-numero codigo.
           rewrite reg-sl01.
           unlock arqsl01 record.
           move "S" to presenca.
           move "N" to etiqueta.
           perform rot-move-sl01.
           write reg-sl01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQSL01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           move zeros to campo-kbd.
           perform dsp-codigo.
           perform until kbd1 not = 0
                   display tela-07
                   perform rot-keypress
                   if kbd1 = 60
                      perform sec-etiqueta
                   end-if
           end-perform.
           perform lmp-codigo thru lmp-presenca.
           display tela-limpa-cad.
           go to lab-inc-01.
      *
       lab-inc-fim.
           perform rot-close-sl01.
           perform rot-close-tabl.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           display tela-11.
           display tela-limpa-cad.
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
                            when kbd2 = 78 or 110
                                 display tela-limpa-cad
                                 perform sec-consulta-nome
                                 display tela-03
                            when kbd2 = 71 or 103
                                 display tela-limpa-cad
                                 perform sec-consulta-grupo
                                 display tela-03
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-fim.
           move zeros to campo-kbd.
           display tela-limpa-cad.
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
           move low-values to sl01-chave.
           move codigo to sl01-codigo.
      *
       lab-cns-codigo-00-a.
           start arqsl01 key is not less sl01-chave.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or sl01-codigo = codigo
              perform rot-inic-arquivo
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if sl01-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqsl01 key is less sl01-chave.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform err-leitura-sl01
              go to lab-cns-codigo-fim
           end-if.
           if sl01-chave = high-values
              perform rot-fim-arquivo
              start arqsl01 key is not less sl01-chave
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
                            perform sec-etiqueta
                            display tela-04
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-codigo-03
                    when kbd-aux = 73
                         go to lab-cns-codigo-01
                    when kbd-aux = 71
                         move low-values to sl01-chave
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to sl01-chave
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-codigo thru lmp-presenca.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-presenca.
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
           move low-values to sl01-chave-1.
           move txt to sl01-empresa in sl01-chave-1.
      *
       lab-cns-empresa-00-a.
           start arqsl01 key is not less sl01-chave-1.
           go to lab-cns-empresa-03.
      *
       lab-cns-empresa-01.
           perform rot-le-anterior.
           if erro not = 0 or sl01-codigo = codigo
              perform rot-inic-arquivo
              move 1 to erro
              go to lab-cns-empresa-05
           end-if.
           if sl01-chave = high-values
              go to lab-cns-empresa-01
           end-if.
           go to lab-cns-empresa-04.
      *
       lab-cns-empresa-02.
           start arqsl01 key is less sl01-chave-1.
      *
       lab-cns-empresa-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform err-leitura-sl01
              go to lab-cns-empresa-fim
           end-if.
           if sl01-chave = high-values
              perform rot-fim-arquivo
              start arqsl01 key is not less sl01-chave-1
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
                            perform sec-etiqueta
                            display tela-04
                            go to lab-cns-empresa-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-empresa-03
                    when kbd-aux = 73
                         go to lab-cns-empresa-01
                    when kbd-aux = 71
                         move low-values to sl01-chave-1
                         go to lab-cns-empresa-00-a
                    when kbd-aux = 79
                         move high-values to sl01-chave-1
                         go to lab-cns-empresa-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-empresa-05
           end-if.
           perform lmp-codigo thru lmp-presenca.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-empresa-00.
      *
       lab-cns-empresa-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-presenca.
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
           move low-values to sl01-chave-2.
           move txt to sl01-nome in sl01-chave-2.
      *
       lab-cns-nome-00-a.
           start arqsl01 key is not less sl01-chave-2.
           go to lab-cns-nome-03.
      *
       lab-cns-nome-01.
           perform rot-le-anterior.
           if erro not = 0 or sl01-codigo = codigo
              perform rot-inic-arquivo
              move 1 to erro
              go to lab-cns-nome-05
           end-if.
           if sl01-chave = high-values
              go to lab-cns-nome-01
           end-if.
           go to lab-cns-nome-04.
      *
       lab-cns-nome-02.
           start arqsl01 key is less sl01-chave-2.
      *
       lab-cns-nome-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform err-leitura-sl01
              go to lab-cns-nome-fim
           end-if.
           if sl01-chave = high-values
              perform rot-fim-arquivo
              start arqsl01 key is not less sl01-chave-3
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
                            perform sec-etiqueta
                            display tela-04
                            go to lab-cns-nome-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-nome-03
                    when kbd-aux = 73
                         go to lab-cns-nome-01
                    when kbd-aux = 71
                         move low-values to sl01-chave-2
                         go to lab-cns-nome-00-a
                    when kbd-aux = 79
                         move high-values to sl01-chave-2
                         go to lab-cns-nome-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-nome-05
           end-if.
           perform lmp-codigo thru lmp-presenca.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-nome-00.
      *
       lab-cns-nome-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-presenca.
           display tela-limpa.
           exit.
      *
       sec-consulta-grupo section.
      *
       lab-cns-grupo-00.
           move spaces to grupo.
           perform lmp-grupo.
           perform acc-grupo.
           if escape-key = 1
              perform lmp-grupo
              go to lab-cns-grupo-fim
           end-if.
           display tela-04.
           move grupo to txt.
           perform rot-texto.
           move low-values to sl01-chave-3.
           move txt to sl01-grupo.
      *
       lab-cns-grupo-00-a.
           start arqsl01 key is not less sl01-chave-3.
           go to lab-cns-grupo-03.
      *
       lab-cns-grupo-01.
           perform rot-le-anterior.
           if erro not = 0 or sl01-codigo = codigo
              perform rot-inic-arquivo
              move 1 to erro
              go to lab-cns-grupo-05
           end-if.
           if sl01-chave = high-values
              go to lab-cns-grupo-01
           end-if.
           go to lab-cns-grupo-04.
      *
       lab-cns-grupo-02.
           start arqsl01 key is less sl01-chave-3.
      *
       lab-cns-grupo-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform err-leitura-sl01
              go to lab-cns-grupo-fim
           end-if.
           if sl01-chave = high-values
              perform rot-fim-arquivo
              start arqsl01 key is not less sl01-chave-3
              move 0 to codigo
              move 1 to erro
              go to lab-cns-grupo-05
           end-if.
      *
       lab-cns-grupo-04.
           perform rot-display.
      *
       lab-cns-grupo-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-grupo-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-grupo-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform sec-etiqueta
                            display tela-04
                            go to lab-cns-grupo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-grupo-03
                    when kbd-aux = 73
                         go to lab-cns-grupo-01
                    when kbd-aux = 71
                         move 0 to sl01-chave-3
                         go to lab-cns-grupo-00-a
                    when kbd-aux = 79
                         move 99 to sl01-chave-3
                         go to lab-cns-grupo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-grupo-05
           end-if.
           perform lmp-codigo thru lmp-presenca.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-grupo-00.
      *
       lab-cns-grupo-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-presenca.
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
           perform rot-le-sl01-lock.
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
           delete arqsl01 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQSL01A.DAT - Tecle <Enter>
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
           unlock arqsl01 record.
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
              go to lab-exc-fim
           end-if.
      *
       lab-alt-00.
           perform rot-le-sl01-lock.
           perform rot-display.
      *
       lab-alt-01.
           perform acc-nome.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           move nome to txt nome-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-01
           end-if.
           move txt to nome sl01-nome in sl01-chave-2.
           perform rot-le-sl01-2.
           if erro = 0 and sl01-codigo not = codigo
              perform err-nome-c-alt
              go to lab-alt-01
           end-if.
           move txt to nome.
      *
       lab-alt-02.
           perform acc-cargo.
           if escape-key = 1
              move nome-a to nome
              go to lab-alt-01
           end-if.
           if cargo = spaces
              go to lab-alt-02
           end-if.
      *
       lab-alt-03.
           perform acc-nome-cracha.
           if escape-key = 1
              go to lab-alt-02
           end-if.
           if nome-cracha = spaces
              go to lab-alt-03
           end-if.
      *
       lab-alt-04.
           perform acc-empresa.
           if escape-key = 1
              go to lab-alt-03
           end-if.
           move empresa to txt empresa-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-04
           end-if.
           move txt to empresa.
      *
       lab-alt-05.
           perform acc-endereco.
           if escape-key = 1
              move empresa-a to empresa
              go to lab-alt-04
           end-if.
           if endereco = spaces 
              go to lab-alt-05
           end-if.
      *
       lab-alt-06.
           perform lmp-cep.
           perform acc-cep.
           if escape-key = 1
              move cep to cep-disp
              perform dsp-cep
              go to lab-alt-05
           end-if.
           if cep = 0
              go to lab-alt-06
           end-if.
           move cep to cep-disp.
           perform dsp-cep.
           move uf to rotina-uf.
      *
       lab-alt-07.
           display tela-08.
           move rotina-uf to uf.
           perform acc-uf.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-06
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-alt-07
           end-if.
           move uf to txt.
           perform rot-texto.
           move txt to uf.
           if uf = spaces
              go to lab-alt-07
           end-if.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move uf to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado
              go to lab-alt-07
           end-if.
           move reg-tabl to reg-wtab03.
           display tela-limpa-cad.
      *
       lab-alt-08.
           perform acc-cidade.
           if escape-key = 1
              go to lab-alt-07
           end-if.
           if cidade = spaces
              go to lab-alt-08
           end-if.
      *
       lab-alt-09.
           perform acc-ddd.
           if escape-key = 1
              go to lab-alt-08
           end-if.
      *
       lab-alt-10.
           perform acc-telefone.
           if escape-key = 1
              go to lab-alt-09
           end-if.
      *
       lab-alt-11.
           perform acc-telex.
           if escape-key = 1
              go to lab-alt-10
           end-if.
      *
       lab-alt-12.
           perform acc-fax.
           if escape-key = 1
              go to lab-alt-11
           end-if.
      *
       lab-alt-13.
           display tela-05.
           perform acc-grupo.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-12
           end-if.
           move grupo to txt.
           perform rot-texto.
           if txt not = "O" and "I" and "F" and "V" and "M" and "A"
              go to lab-alt-13
           end-if.
           move txt to grupo.
           perform dsp-grupo.
           display tela-limpa-cad.
      *
       lab-alt-14.
           perform acc-tkt.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-13
           end-if.
           move tkt to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-14
           end-if.
           move txt to tkt.
           perform dsp-tkt.
      *
       lab-alt-15.
           display tela-09.
           perform acc-presenca.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-14
           end-if.
           move presenca to txt.
           perform rot-texto.
           if txt not = "S" and "N"
              go to lab-alt-15
           end-if.
           move txt to presenca.
           perform dsp-presenca.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-alt-16.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-15
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-16
              end-if
           end-if.
           perform rot-move-sl01.
           rewrite reg-sl01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQSL01A.DAT - Tecle <Ent
      -            "er>" to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-alt-fim
           end-rewrite.
           move zeros to campo-kbd.
      *
       lab-alt-fim.
           unlock arqsl01 record.
           display tela-04.
           exit.
      *
       sec-etiqueta section.
      *
       lab-etq-00.
           move 0 to erro.
           perform rot-ponteiro.
           perform rot-le-sl01-lock.
           if erro not = 0
              perform err-leitura-sl01
              go to lab-etq-fim
           end-if.
           move param-impress to impress.
           perform rot-open-imp.
           if erro not = 0
              go to lab-etq-fim
           end-if.
      *
       lab-etq-01.
           move "N" to flag-empresa.
           perform rot-move-campos.
           write reg-imp from cab-01 after 0 line.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-03 after 1 line.
           write reg-imp from cab-04 after 1 line.
           write reg-imp from cab-05 after 1 line.
           write reg-imp from spaces after 2 line.
           move "S" to etiqueta.
           if sl01-etiqueta not = "S"
              move "S" to sl01-etiqueta
              rewrite reg-sl01 invalid key
                      move 
                      " Erro de regravacao no ARQSL01A.DAT - Tecle <Ente
      -               "r>" to mensagem
                      display tela-erro
                      perform rot-keypress
                      display tela-limpa
                      go to lab-etq-fim
              end-rewrite
           end-if.
      *
       lab-etq-fim.
           unlock arqsl01 record.
           move zeros to campo-kbd.
           perform rot-close-imp.
           exit.
      *
       sec-inclusao-c section.
      *
       lab-inc-00-c.
      *
       lab-inc-01-c.
           display tela-12.
           move spaces to nome.
           perform lmp-nome.
           perform acc-nome.
           if escape-key = 1
              go to lab-inc-fim-c
           end-if.
           move nome to txt nome-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-01-c
           end-if.
           move txt to nome sl01-nome in sl01-chave-2.
           perform rot-le-sl01-2.
           if erro = 0
              perform err-nome-c
              go to lab-inc-01-c
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-02-c.
           move spaces to nome-cracha.
           perform lmp-nome-cracha.
           perform acc-nome-cracha.
           if escape-key = 1
              perform lmp-nome-cracha
              go to lab-inc-01-c
           end-if.
           if nome-cracha = spaces
              go to lab-inc-02-c
           end-if.
      *
       lab-inc-03-c.
           move spaces to empresa.
           perform lmp-empresa.
           perform acc-empresa.
           if escape-key = 1
              perform lmp-empresa
              go to lab-inc-02-c
           end-if.
           move empresa to txt empresa-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-03-c
           end-if.
           move txt to sl01-empresa in sl01-chave-1 empresa.
      *     move 0 to erro.
      *     start arqsl01 key is equal sl01-chave-1 
      *           invalid key move 1 to erro
      *     end-start.
      *     if erro = 0
      *        perform rot-le-proximo
      *        perform rot-move-campos
      *        perform dsp-endereco thru dsp-tkt
      *        go to lab-inc-bosta
      *     end-if.
      *
            move spaces to rotina-uf.
      *
       lab-inc-04-c.
           display tela-08.
           move rotina-uf to uf.
           perform lmp-uf
           perform acc-uf.
           if escape-key = 1
              perform lmp-uf
              display tela-limpa-cad
              go to lab-inc-03-c
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-inc-04-c
           end-if.
           move uf to txt.
           perform rot-texto.
           move txt to uf.
           if uf = spaces
              go to lab-inc-04-c
           end-if.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move uf to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado
              go to lab-inc-04-c
           end-if.
           move reg-tabl to reg-wtab03.
           display tela-limpa-cad.
      *
       lab-inc-05-c.
           move wtab03-capital to cidade.
           perform lmp-cidade.
           perform acc-cidade.
           if escape-key = 1
              perform lmp-cidade
              go to lab-inc-04-c
           end-if.
           if cidade = spaces
              go to lab-inc-05-c
           end-if.
           move cidade to wtab03-capital.
      *
       lab-inc-06-c.
           display tela-05.
           move space to grupo.
           perform lmp-grupo.
           perform acc-grupo.
           if escape-key = 1
              perform lmp-grupo
              display tela-limpa-cad
              go to lab-inc-05-c
           end-if.
           move grupo to txt.
           perform rot-texto.
           if txt not = "O" and "I" and "F" and "V" and "M" and "A"
              go to lab-inc-06-c
           end-if.
           move txt to grupo.
           perform dsp-grupo.
           display tela-limpa-cad.
      *
       lab-inc-07-c.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-06-c
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-codigo thru lmp-presenca
              go to lab-inc-01-c
           else
              if resposta not = "S"
                 go to lab-inc-07-c
              end-if
           end-if.
           move high-values to sl01-chave-controle.
           perform rot-ponteiro.
           perform rot-le-sl01-lock.
           if erro not = 0
              move " Erro no registro de controle - ARQSL01A.DAT - Tecle
      -       " <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              go to lab-inc-fim
           end-if.
           move sl01-numero to codigo.
           add 1 to sl01-numero codigo.
           rewrite reg-sl01.
           unlock arqsl01 record.
           move "." to cargo.
           move "." to endereco.
           move 1 to cep.
           move "." to telefone.
           move "." to telex.
           move "." to fax.
           move "*" to tkt.
           move "S" to presenca.
           move "N" to etiqueta.
           perform rot-move-sl01.
           write reg-sl01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQSL01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           move zeros to campo-kbd.
           perform dsp-codigo.
           perform until kbd1 not = 0
                   display tela-07
                   perform rot-keypress
                   if kbd1 = 60
                      perform sec-etiqueta
                   end-if
           end-perform.
           perform lmp-codigo thru lmp-presenca.
           display tela-limpa-cad.
           go to lab-inc-01-c.
      *
       lab-inc-fim-c.
           exit.