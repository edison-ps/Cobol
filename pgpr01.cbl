      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGPR01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao da Pesquisa de Receptivo :                      *
      *                                                             *
      *  Data da ultima alteracao:    10/11/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgpr01.
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
           select arqat02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is at02-chave
                  file status is at02-status.
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
       copy fdat02.lib.
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
       01 at02-status                  pic x(02) value "00".
       01 at02-stat                    pic x(01) value "F".
      *
       01 nome-arq-at02.
          02 at02-dir                  pic x(03) value "AT2".
          02 filler                    pic x(01) value "\".
          02 at02-nome                 pic x(08) value "ARQAT02A".
          02 filler                    pic x(01) value ".".
          02 at02-ext                  pic x(03) value "DAT".
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
          02 cb-programa               pic x(08) value "PGPR01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(40) value spaces.
       01 limpa-01                     pic x(01) value spaces.
       01 limpa-03                     pic x(03) value spaces.
       01 limpa-06                     pic x(06) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 sub                          pic 9(01) value 0.
       01 sub-aux                      pic 9(01) value 0.
       01 sub-tela                     pic 9(02) value 0.
       01 valor-aux                    pic zz9,99 value 0.
       01 total-aux                    pic 9(03)v99 value 0.
       01 total-aux-01                 pic 9(04) value 0.
      *
       01 campos.
          02 codigo                    pic x(06) value spaces.
          02 empresa                   pic x(40) value spaces.
          02 estrangeiros              pic x(01) value spaces.
          02 passageiros.
             03 pax-i                  pic 9(03)v9(02) value 0.
             03 pax-g                  pic 9(03)v9(02) value 0.
          02 area-procedencia.
             03 am-norte               pic 9(03)v9(02) value 0.
             03 am-central             pic 9(03)v9(02) value 0.
             03 am-sul                 pic 9(03)v9(02) value 0.
             03 europa                 pic 9(03)v9(02) value 0.
             03 asia                   pic 9(03)v9(02) value 0.
             03 africa                 pic 9(03)v9(02) value 0.
          02 transportes.
             03 transp occurs 6 times.
                04 tipo-t              pic 9(03).
                04 proprio-t           pic 9(03).
                04 terceiros-t         pic 9(03).
          02 desc-t                    pic x(30) occurs 5.
          02 funcionarios              pic 9(03) value 0.
          02 funcionarios-r            pic 9(03) value 0.
          02 guias.
             03 guias-a                pic 9(03) value 0.
             03 guias-c                pic 9(03) value 0.
          02 faturamento               pic x(01) value spaces.
          02 fat.
             03 fat-resp               pic x(01) value spaces.
             03 fat-valor              pic 9(03)v9(02) value 0.
          02 sugestao.
             03 sug-01                 pic x(40) value spaces.
             03 sug-02                 pic x(40) value spaces.
             03 sug-03                 pic x(40) value spaces.
             03 sug-04                 pic x(40) value spaces.
      *
       01 campo-aux.
          02 tipo-t-aux                pic 9(03) value 0.
          02 desc-t-aux                pic x(30) value spaces.
          02 proprio-t-aux             pic 9(03) value 0.
          02 terceiros-t-aux           pic 9(03) value 0.
      *
       01 buffer-01 value zeros.
          02 filler                    pic 9(04) occurs 896.
      *
       01 buffer-02 value zeros.
          02 filler                    pic 9(04) occurs 684.
      *
       01 buffer-03 value zeros.
          02 filler                    pic 9(04) occurs 1040.
      *
       01 buffer-04 value zeros.
          02 filler                    pic 9(04) occurs 1156.
      *
       01 buffer-05 value zeros.
          02 filler                    pic 9(04) occurs 550.
      *
       01 buffer-06 value zeros.
          02 filler                    pic 9(04) occurs 550.
      *
       01 buffer-07 value zeros.
          02 filler                    pic 9(04) occurs 649.
      *
       01 buffer-08 value zeros.
          02 filler                    pic 9(04) occurs 720.
      *
       01 buffer-09 value zeros.
          02 filler                    pic 9(04) occurs 765.
      *
       01 buffer-10 value zeros.
          02 filler                    pic 9(04) occurs 754.
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
          02 rotina-codigo-cod         pic x(06) value spaces.
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
       01 cab-usr.
          02 filler                    pic x(10) value "Usuario.:".
          02 cab-usuario               pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Data....:".
          02 cab-data                  pic x(08) value spaces.
      *
       copy wstab01.lib.
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
          02 line 13 column 07 foreground-color 06 background-color 04
             value "Codigo....:".
          02 line 14 column 07 foreground-color 06 background-color 04
             value "Empresa...:".
      *
       01 tela-02.
          02 line 16 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 16 column 05 foreground-color 07 background-color 02
             highlight value "F1".
          02 line 16 column 07 foreground-color 01 background-color 02
             value "-Help".
          02 line 16 column 15 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 16 column 17 foreground-color 01 background-color 02
             value "-Consultas".
      *
       01 tela-04.
          02 line 16 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from spaces.
          02 line 16 column 06 foreground-color 07 background-color 02 
             highlight value "F2".
          02 line 16 column 08 foreground-color 01 background-color 02
             value "-Alt".
          02 line 16 column 14 foreground-color 07 background-color 02 
             highlight value "F3".
          02 line 16 column 16 foreground-color 01 background-color 02
             value "-Exc".
          02 line 16 column 22 foreground-color 07 background-color 02 
             highlight value "F4".
          02 line 16 column 24 foreground-color 01 background-color 02
             value "-Dados".
          02 line 16 column 32 foreground-color 07 background-color 02
             highlight value "Home".
          02 line 16 column 36 foreground-color 01 background-color 02
             value "-Inic".
          02 line 16 column 43 foreground-color 07 background-color 02
             highlight value "End".
          02 line 16 column 46 foreground-color 01 background-color 02
             value "-Fim".
          02 line 16 column 52 foreground-color 07 background-color 02
             highlight value "PgDown".
          02 line 16 column 58 foreground-color 01 background-color 02
             value "-Prox".
          02 line 16 column 64 foreground-color 07 background-color 02
             highlight value "PgUp".
          02 line 16 column 68 foreground-color 01 background-color 02
             value "-Ant".
      *
       01 tela-05.
          02 line 21 column 07 foreground-color 07 background-color 02
             highlight pic x(63) from spaces.
          02 line 21 column 07 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 21 column 09 foreground-color 01 background-color 02
             value " - Veiculos".
      *
       01 tela-06.
          02 line 20 column 13 foreground-color 07 background-color 02
             highlight pic x(53) from spaces.
          02 line 20 column 13 foreground-color 07 background-color 02
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-09.
          02 line 12 column 65 foreground-color 07 background-color 04
             highlight value "Inclusao".
      *
       01 tela-10.
          02 line 12 column 65 foreground-color 07 background-color 04
             highlight value "Consulta".
      *
       01 tela-11.
          02 line 12 column 10 foreground-color 06 background-color 04
             value "1) Qual o numero aproximado de turistas estrangeiros
      -      " ".
          02 line 13 column 13 foreground-color 06 background-color 04
             value "que sua Agencia recebe anualmente?".
          02 line 15 column 13 foreground-color 06 background-color 04
             value "(A)    ate    1.000          (B)  1.000 a  3.000".
          02 line 16 column 13 foreground-color 06 background-color 04
             value "(C)  3.001 a  5.000          (D)  5.001 a 10.000".
          02 line 17 column 13 foreground-color 06 background-color 04
             value "(E) 10.001 a 15.000          (F)  mais de 15.000".
          02 line 19 column 13 foreground-color 06 background-color 04
             value "Alternativa escolhida......:".
      *
       01 tela-12.
          02 line 11 column 16 foreground-color 06 background-color 04
             value "2) Especifique a divisao provavel, por tipo de".
          02 line 12 column 19 foreground-color 06 background-color 04
             value "passageiros:".
          02 line 14 column 19 foreground-color 06 background-color 04
             value "Individuais:          %".
          02 line 15 column 19 foreground-color 06 background-color 04
             value "Grupo......:          %".
          02 line 16 column 19 foreground-color 06 background-color 04
             value "Total......:          %".
      *
       01 tela-13.
          02 line 10 column 10 foreground-color 06 background-color 04
             value "3) Especifique a divisao por area de procedencia:".
          02 line 13 column 13 foreground-color 06 background-color 04
             value "Estados Unidos / Canada..:         %".
          02 line 14 column 13 foreground-color 06 background-color 04
             value "Mexico / America Central.:         %".
          02 line 15 column 13 foreground-color 06 background-color 04
             value "America do Sul...........:         %".
          02 line 16 column 13 foreground-color 06 background-color 04
             value "Europa...................:         %".
          02 line 17 column 13 foreground-color 06 background-color 04
             value "Asia e Extremo Oriente...:         %".
          02 line 18 column 13 foreground-color 06 background-color 04
             value "Africa...................:         %".
          02 line 19 column 13 foreground-color 06 background-color 04
             value "Total....................:         %".
      *
       01 tela-14.
          02 line 09 column 10 foreground-color 06 background-color 04
             value "4) Quais os tipos de transportes que sua Agencia uti
      -      "liza:".
          02 line 11 column 13 foreground-color 06 background-color 04
             value "TIPO:                               QUANTIDADE:".
          02 line 12 column 46 foreground-color 06 background-color 04
             value "PROPRIO     TERCEIROS".
          02 line 19 column 14 foreground-color 06 background-color 04
             value "Total:".
      *
       01 tela-15.
          02 line 11 column 18 foreground-color 06 background-color 04
             value "5) Qual o numero de funcionarios contratados".
          02 line 12 column 21 foreground-color 06 background-color 04
             value "por sua Agencia?".
          02 line 14 column 21 foreground-color 06 background-color 04
             value "Total......:".
      *
       01 tela-16.
          02 line 13 column 16 foreground-color 06 background-color 04
             value "6) Qual o numero de funcionarios envolvidos".
          02 line 14 column 19 foreground-color 06 background-color 04
             value "exclusivamente com receptivo?".
          02 line 16 column 19 foreground-color 06 background-color 04
             value "Total......:".
      *
       01 tela-17.
          02 line 11 column 16 foreground-color 06 background-color 04
             value "7) Quantos guias sao utilizados por sua Agencia?".
          02 line 13 column 19 foreground-color 06 background-color 04
             value "Autonomos....:".
          02 line 14 column 19 foreground-color 06 background-color 04
             value "Contratados..:".
          02 line 15 column 19 foreground-color 06 background-color 04
             value "Total........:".
      *
       01 tela-18.
          02 line 10 column 14 foreground-color 06 background-color 04
             value "8) A partir da implantacao do Real, o".
          02 line 11 column 17 foreground-color 06 background-color 04
             value "faturamento da sua Agencia:".
          02 line 14 column 17 foreground-color 06 background-color 04
             value "(A) Diminuiu".
          02 line 15 column 17 foreground-color 06 background-color 04
             value "(B) Permaneceu igual".
          02 line 16 column 17 foreground-color 06 background-color 04
             value "(C) Aumentou".
          02 line 18 column 17 foreground-color 06 background-color 04
             value "Alternativa escolhida......:".
      *
       01 tela-19.
          02 line 13 column 21 foreground-color 06 background-color 04
             value "9) Qual a sua expectativa, quanto ao".
          02 line 14 column 24 foreground-color 06 background-color 04
             value "faturamento no proximo ano?".
          02 line 16 column 24 foreground-color 06 background-color 04
             value "(A) Deve piorar.Em Quanto?           %".
          02 line 17 column 24 foreground-color 06 background-color 04
             value "(B) Deve permanecer igual.".
          02 line 18 column 24 foreground-color 06 background-color 04
             value "(C) Deve melhorar.Em Quanto?         %".
          02 line 20 column 24 foreground-color 06 background-color 04
             value "Alternativa escolhida......:".
      *
       01 tela-20.
          02 line 12 column 15 foreground-color 06 background-color 04
             value "10) Analise e de sugestoes, caso deseje, sobre as".
          02 line 13 column 22 foreground-color 06 background-color 04
             value "pesquisas, formulacao das perguntas, etc.".
      *
       01 tela-mensagem-cad.
          02 line 16 column 05 foreground-color 07 background-color 02
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 16 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 16 column 05 foreground-color 04 background-color 04
             pic x(68) from spaces.
      *
       01 tela-mensagem-cad-04.
          02 line 21 column 07 foreground-color 07 background-color 02
             highlight pic x(63) from mensagem.
      *
       01 tela-erro-cad-04.
          02 line 21 column 07 beep reverse-video pic x(63) from 
             mensagem.
      *
       01 tela-limpa-cad-04.
          02 line 21 column 07 foreground-color 04 background-color 04
             pic x(63) from spaces.
      *
       01 tela-mensagem-cad-10.
          02 line 20 column 13 foreground-color 07 background-color 02
             highlight pic x(53) from mensagem.
      *
       01 tela-erro-cad-10.
          02 line 20 column 13 beep reverse-video pic x(53) from 
             mensagem.
      *
       01 tela-limpa-cad-10.
          02 line 20 column 13 foreground-color 04 background-color 04
             pic x(53) from spaces.
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
           move 10 to box-lin.
           move 72 to box-col-f.
           move 16 to box-lin-f.
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
       rot-move-at02.
           move codigo to at02-codigo.
           move estrangeiros to at02-estrangeiros.
           move passageiros to at02-passageiros.
           move area-procedencia to at02-area-procedencia.
           move transportes to at02-transportes.
           move funcionarios to at02-funcionarios.
           move funcionarios to at02-funcionarios-r.
           move guias to at02-guias.
           move faturamento to at02-faturamento.
           move fat to at02-fat.
           move sugestao to at02-sugestao.
           move param-usr to at02-usuario.
           move param-data to at02-data.
      *
       rot-move-campos.
           move at02-codigo to codigo.
           move at02-estrangeiros to estrangeiros.
           move at02-passageiros to passageiros.
           move at02-area-procedencia to area-procedencia.
           move at02-transportes to transportes.
           move at02-funcionarios to funcionarios.
           move at02-funcionarios to funcionarios-r.
           move at02-guias to guias.
           move at02-faturamento to faturamento.
           move at02-fat to fat.
           move at02-sugestao to sugestao.
           move at02-data to dias-corr.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move at02-usuario to cab-usuario.
      *
       rot-le-at01.
           move 0 to erro.
           read arqat01 invalid key move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-at01.
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
       rot-le-at02.
           move 0 to erro.
           read arqat02 invalid key move 1 to erro.
           if at02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-at02.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqat02 key is equal at02-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-at02
           end-start.
      *
       rot-le-at02-lock.
           move 0 to erro.
           read arqat02 next. 
           if at02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-at02-lock
           end-if.
           read arqat02 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqat02 previous at end move 1 to erro.
           if at02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqat02 next at end move 1 to erro.
           if at02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-at02.
           move 0 to erro.
           if at02-stat = "F"
              open i-o arqat02
              if at02-status not = "00"
                 move 
                 " Erro de abertura no ARQAT02A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
                 move zeros to reg-at02
                 move high-values to at02-controle
                 write reg-at02-1
               else
                  move "A" to at01-stat
               end-if
           end-if.
      *
       rot-close-at02.
           if at02-stat = "A"
              close arqat02
              move "F" to at02-stat
           end-if.
      *
       rot-erro-leitura-at02.
           move " Erro de leitura - ARQAT02A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.

      *
       rot-inic-arquivo.
           perform lmp-codigo thru lmp-empresa.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-codigo thru lmp-empresa.
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
           move at02-codigo to at01-codigo.
           perform rot-le-at01.
           if erro not = 0
              move "Registro nao cadastrado" to empresa
           else
              move at01-empresa-a to empresa
           end-if.
           perform dsp-codigo thru dsp-empresa.
           perform rot-le-veiculos.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-search.
           move 1 to erro.
           move 0 to sub.
           perform until sub = 6
                   add 1 to sub
                   if tipo-t (sub) = tipo-t-aux and sub-aux not = sub
                      move 0 to erro
                   end-if
           end-perform.
      *
       rot-remonta.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-rest-buffer.
           display tela-cabec.
           move 03 to box-col.
           move 10 to box-lin.
           move 72 to box-col-f.
           move 16 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-01.
      *
       rot-display-tela.
           move 1 to sub-tela.
           move zeros to campo-aux buffer-01 buffer-02 buffer-03
                         buffer-04 buffer-05 buffer-06 buffer-07
                         buffer-08 buffer-09 buffer-10.
           perform until kbd-aux = 1
                   evaluate true
                            when sub-tela = 1 
                                 perform rot-display-tela-01 
                                 perform dsp-estrangeiros
                            when sub-tela = 2
                                 perform rot-display-tela-02 
                                 perform dsp-pax-i thru dsp-pax-total
                            when sub-tela = 3 
                                 perform rot-display-tela-03 
                                 perform dsp-am-norte thru 
                                         dsp-mundo-total
                            when sub-tela = 4 
                                 perform rot-display-tela-04
                                 perform dsp-tipo-t-01 thru 
                                         dsp-terceiros-total
                            when sub-tela = 5
                                 perform rot-display-tela-05 
                                 perform dsp-funcionarios
                            when sub-tela = 6
                                 perform rot-display-tela-06 
                                 perform dsp-funcionarios-r
                            when sub-tela = 7
                                 perform rot-display-tela-07 
                                 perform dsp-guias-a thru 
                                         dsp-guias-total
                            when sub-tela = 8
                                 perform rot-display-tela-08 
                                 perform dsp-faturamento
                            when sub-tela = 9
                                 perform rot-display-tela-09 
                                 perform dsp-fat-resp thru 
                                         dsp-fat-valor
                            when sub-tela = 10
                                 perform rot-display-tela-10 
                                 perform dsp-sug-01 thru 
                                         dsp-sug-04
                   end-evaluate
                   perform rot-keypress
                   move kbd1 to kbd-aux
                   evaluate true
                            when sub-tela = 1 
                                 perform rot-restore-tela-01 
                                 move zeros to buffer-01
                            when sub-tela = 2
                                 perform rot-restore-tela-02 
                                 move zeros to buffer-02
                            when sub-tela = 3 
                                 perform rot-restore-tela-03 
                                 move zeros to buffer-03
                            when sub-tela = 4 
                                 perform rot-restore-tela-04
                                 move zeros to buffer-04
                            when sub-tela = 5
                                 perform rot-restore-tela-05 
                                 move zeros to buffer-05
                            when sub-tela = 6
                                 perform rot-restore-tela-06 
                                 move zeros to buffer-06
                            when sub-tela = 7
                                 perform rot-restore-tela-07 
                                 move zeros to buffer-07
                            when sub-tela = 8
                                 perform rot-restore-tela-08 
                                 move zeros to buffer-08
                            when sub-tela = 9
                                 perform rot-restore-tela-09 
                                 move zeros to buffer-09
                            when sub-tela = 10
                                 perform rot-restore-tela-10 
                                 move zeros to buffer-10
                   end-evaluate
                   evaluate true
                            when kbd-aux = 81 
                                 if sub-tela < 10
                                    add 1 to sub-tela
                                 end-if
                            when kbd-aux = 73
                                 if sub-tela > 1
                                    subtract 1 from sub-tela
                                 end-if
                   end-evaluate
           end-perform.
      *
       rot-display-tela-01.
           move 05 to box-col.
           move 09 to box-lin.
           if buffer-01 = zeros
              move 67 to box-col-f
              move 22 to box-lin-f
              call "C_Savescr" using by value box-col
                                     by value box-lin
                                     by value box-col-f
                                     by value box-lin-f
                                     by reference buffer-01
           end-if.
           move 65 to box-col-f.
           move 21 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-11.
      *
       rot-restore-tela-01.
           move 67 to box-col-f.
           move 22 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer-01.
      *
       rot-display-tela-02.
           move 11 to box-col.
           move 08 to box-lin.
           if buffer-02 = zeros
              move 66 to box-col-f
              move 19 to box-lin-f
              call "C_Savescr" using by value box-col
                                     by value box-lin
                                     by value box-col-f
                                     by value box-lin-f
                                     by reference buffer-02
           end-if.
           move 64 to box-col-f.
           move 18 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-12.
      *
       rot-restore-tela-02.
           move 66 to box-col-f.
           move 19 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer-02.
      *
       rot-display-tela-03.
           move 05 to box-col.
           move 07 to box-lin.
           if buffer-03 = zeros           
              move 68 to box-col-f
              move 22 to box-lin-f
              call "C_Savescr" using by value box-col
                                     by value box-lin
                                     by value box-col-f
                                     by value box-lin-f
                                     by reference buffer-03
           end-if.
           move 66 to box-col-f.
           move 21 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-13.
      *
       rot-restore-tela-03.
           move 68 to box-col-f
           move 22 to box-lin-f
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer-03.
      *
       rot-display-tela-04.
           move 05 to box-col.
           move 06 to box-lin.
           if buffer-04 = zeros
              move 72 to box-col-f
              move 23 to box-lin-f
              call "C_Savescr" using by value box-col
                                     by value box-lin
                                     by value box-col-f
                                     by value box-lin-f
                                     by reference buffer-04
           end-if.
           move 70 to box-col-f.
           move 22 to box-lin-f.
           move 69 to box-col-f.
           move 21 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-14.
      *
       rot-restore-tela-04.
           move 72 to box-col-f.
           move 23 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer-04.
      *
       rot-display-tela-05.
           move 13 to box-col.
           move 08 to box-lin.
           if buffer-05 = zeros
              move 66 to box-col-f
              move 17 to box-lin-f
              call "C_Savescr" using by value box-col
                                     by value box-lin
                                     by value box-col-f
                                     by value box-lin-f
                                     by reference buffer-05
           end-if.
           move 64 to box-col-f.
           move 16 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-15.
      *
       rot-restore-tela-05.
           move 66 to box-col-f.
           move 17 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer-05.
      *
       rot-display-tela-06.
           move 11 to box-col.
           move 10 to box-lin.
           if buffer-06 = zeros
              move 64 to box-col-f
              move 19 to box-lin-f
              call "C_Savescr" using by value box-col
                                     by value box-lin
                                     by value box-col-f
                                     by value box-lin-f
                                     by reference buffer-06
           end-if.
           move 62 to box-col-f.
           move 18 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-16.
      *
       rot-restore-tela-06.
           move 64 to box-col-f.
           move 19 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer-06.
      *
       rot-display-tela-07.
           move 11 to box-col.
           move 08 to box-lin.
           if buffer-07 = zeros
              move 68 to box-col-f
              move 18 to box-lin-f
              call "C_Savescr" using by value box-col
                                     by value box-lin
                                     by value box-col-f
                                     by value box-lin-f
                                     by reference buffer-07
           end-if.
           move 66 to box-col-f.
           move 17 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-17.
      *
       rot-restore-tela-07.
           move 68 to box-col-f.
           move 18 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer-07.
      *
       rot-display-tela-08.
           move 09 to box-col.
           move 07 to box-lin.
           if buffer-08 = zeros
              move 55 to box-col-f
              move 21 to box-lin-f
              call "C_Savescr" using by value box-col
                                     by value box-lin
                                     by value box-col-f
                                     by value box-lin-f
                                     by reference buffer-08
           end-if.
           move 53 to box-col-f.
           move 20 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-18.
      *
       rot-restore-tela-08.
           move 55 to box-col-f.
           move 21 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer-08.
      *
       rot-display-tela-09.
           move 16 to box-col.
           move 10 to box-lin.
           if buffer-09 = zeros
              move 65 to box-col-f
              move 23 to box-lin-f
              call "C_Savescr" using by value box-col
                                     by value box-lin
                                     by value box-col-f
                                     by value box-lin-f
                                     by reference buffer-09
           end-if.
           move 63 to box-col-f.
           move 22 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-19.
      *
       rot-restore-tela-09.
           move 65 to box-col-f.
           move 23 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer-09.
      *
       rot-display-tela-10.
           move 11 to box-col.
           move 09 to box-lin.
           if buffer-10 = zeros
              move 67 to box-col-f
              move 21 to box-lin-f
              call "C_Savescr" using by value box-col
                                     by value box-lin
                                     by value box-col-f
                                     by value box-lin-f
                                     by reference buffer-10
           end-if.
           move 65 to box-col-f.
           move 20 to box-lin-f.
           move "3" to box-borda.
           move 04 to box-cor-f.
           move 10 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-20.
      *
       rot-restore-tela-10.
           move 67 to box-col-f.
           move 21 to box-lin-f.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer-10.
      *
       rot-le-veiculos.
           move 1 to sub.
           move 0 to tipo-t (06).
           perform until tipo-t (sub) = 0
                   move 07 to wtab01-tipo
                   move tipo-t (sub) to wtab01-codigo rotina-codigo
                   move spaces to wtab01-resto
                   move wtab01-chave to tabl-chave
                   perform rot-le-tabl
                   if erro not = 0
                      move spaces to desc-t (sub)
                   else
                      move reg-tabl to reg-wtab01
                      move wtab01-descricao to desc-t (sub)
                   end-if
                   add 1 to sub
           end-perform.
      *
       rot-pesq-agencia.
           perform rot-close-at01.
           move 15 to rotina-col-cod.
           move 13 to rotina-lin-cod.
           move "3" to rotina-borda-cod.
           move spaces to rotina-fundo-cod.
           move "S" to rotina-sombra-cod.
           call "rotat01" using param-menu campo-rotina-cod.
           cancel "rotat01".
           perform rot-open-at01.
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
       err-codigo-n.
           move " Codigo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-veiculo-n.
           move " Veiculo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad-04.
           perform rot-keypress.
           display tela-limpa-cad-04.
      *
       err-atividade.
           move " Agencia nao faz turismo receptivo - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
      *  Sequencia para dar Accept
      *
       acc-codigo.
           accept codigo at 1319 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-empresa.
           accept empresa at 1419 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-estrangeiros.
           accept estrangeiros at 1942 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-pax-i.
           move pax-i to valor-aux.
           accept valor-aux at 1432 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move valor-aux to pax-i.
           exit.
      *
       acc-pax-g.
           move pax-g to valor-aux.
           accept valor-aux at 1532 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move valor-aux to pax-g.
           exit.
      *
       acc-am-norte.
           move am-norte to valor-aux.
           accept valor-aux at 1340 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move valor-aux to am-norte.
           exit.
      *
       acc-am-central.
           move am-central to valor-aux.
           accept valor-aux at 1440 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move valor-aux to am-central
           exit.
      *
       acc-am-sul.
           move am-sul to valor-aux.
           accept valor-aux at 1540 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move valor-aux to am-sul.
           exit.
      *
       acc-europa.
           move europa to valor-aux.
           accept valor-aux at 1640 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move valor-aux to europa.
           exit.
      *
       acc-asia.
           move asia to valor-aux.
           accept valor-aux at 1740 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move valor-aux to asia.
           exit.
      *
       acc-africa.
           move africa to valor-aux.
           accept valor-aux at 1840 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           move valor-aux to africa.
           exit.
      *
       acc-tipo-t-01.
           accept tipo-t (01) at 1410 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-proprio-t-01.
           accept proprio-t (01) at 1450 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-terceiros-t-01.
           accept terceiros-t (01) at 1464 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-tipo-t-02.
           accept tipo-t (02) at 1510 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-proprio-t-02.
           accept proprio-t (02) at 1550 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-terceiros-t-02.
           accept terceiros-t (02) at 1564 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-tipo-t-03.
           accept tipo-t (03) at 1610 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-proprio-t-03.
           accept proprio-t (03) at 1650 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-terceiros-t-03.
           accept terceiros-t (03) at 1664 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-tipo-t-04.
           accept tipo-t (04) at 1710 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-proprio-t-04.
           accept proprio-t (04) at 1750 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-terceiros-t-04.
           accept terceiros-t (04) at 1764 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-tipo-t-05.
           accept tipo-t (05) at 1810 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-proprio-t-05.
           accept proprio-t (05) at 1850 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-terceiros-t-05.
           accept terceiros-t (05) at 1864 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-funcionarios.
           accept funcionarios at 1434 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-funcionarios-r.
           accept funcionarios-r at 1632 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-guias-a.
           accept guias-a at 1335 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-guias-c.
           accept guias-c at 1435 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-faturamento.
           accept faturamento at 1846 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-fat-resp.
           accept fat-resp at 2053 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-fat-valor.
           move fat-valor to valor-aux.
           if fat-resp = "A"
              accept valor-aux at 1654 with auto update prompt
                     foreground-color 15 background-color 04
              accept escape-key from escape
              move valor-aux to fat-valor
              exit
           else
              accept valor-aux at 1854 with auto update prompt
                     foreground-color 15 background-color 04
              accept escape-key from escape
              move valor-aux to fat-valor
              exit
           end-if.
      *
       acc-sug-01.
           accept sug-01 at 1519 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-sug-02.
           accept sug-02 at 1619 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-sug-03.
           accept sug-03 at 1719 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-sug-04.
           accept sug-04 at 1819 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo.
           display codigo at 1319 with foreground-color 15 
                   background-color 04.
      *
       dsp-empresa.
           display empresa at 1419 with foreground-color 15 
                   background-color 04.
      *
       dsp-estrangeiros.
           display estrangeiros at 1942 with foreground-color 15 
                   background-color 04.
      *
       dsp-pax-i.
           move pax-i to valor-aux.
           display valor-aux at 1432 with foreground-color 15 
                   background-color 04.
      *
       dsp-pax-g.
           move pax-g to valor-aux.
           display valor-aux at 1532 with foreground-color 15 
                   background-color 04.
      *
       dsp-pax-total.
           move 0 to total-aux.
           add pax-i pax-g to total-aux.
           move total-aux to valor-aux.
           display valor-aux at 1632 with foreground-color 15 
                   background-color 04.
      *
       dsp-am-norte.
           move am-norte to valor-aux.
           display valor-aux at 1340 with foreground-color 15 
                   background-color 04.
      *
       dsp-am-central.
           move am-central to valor-aux.
           display valor-aux at 1440 with foreground-color 15 
                   background-color 04.
      *
       dsp-am-sul.
           move am-sul to valor-aux.
           display valor-aux at 1540 with foreground-color 15 
                   background-color 04.
      *
       dsp-europa.
           move europa to valor-aux.
           display valor-aux at 1640 with foreground-color 15 
                   background-color 04.
      *
       dsp-asia.
           move asia to valor-aux
           display valor-aux at 1740 with foreground-color 15 
                   background-color 04.
      *
       dsp-africa.
           move africa to valor-aux.
           display valor-aux at 1840 with foreground-color 15 
                   background-color 04.
      *
       dsp-mundo-total.
           move 0 to total-aux.
           add am-norte am-central am-sul europa 
               asia africa to total-aux
           move total-aux to valor-aux.
           display valor-aux at 1940 with foreground-color 15 
                   background-color 04.
      *
       dsp-tipo-t-01.
           if tipo-t (01) not = 0
              display tipo-t (01) at 1410 with foreground-color 15 
                       background-color 04
              display desc-t (01) at 1414 with foreground-color 15 
                   background-color 04
           end-if.
      *
       dsp-proprio-t-01.
           if tipo-t (01) not = 0
              display proprio-t (01) at 1450 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-terceiros-t-01.
           if tipo-t (01) not = 0
              display terceiros-t (01) at 1464 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-tipo-t-02.
           if tipo-t (02) not = 0
              display tipo-t (02) at 1510 with foreground-color 15 
                      background-color 04
              display desc-t (02) at 1514 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-proprio-t-02.
           if tipo-t (02) not = 0
              display proprio-t (02) at 1550 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-terceiros-t-02.
           if tipo-t (02) not = 0
              display terceiros-t (02) at 1564 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-tipo-t-03.
           if tipo-t (03) not = 0
              display tipo-t (03) at 1610 with foreground-color 15 
                      background-color 04
              display desc-t (03) at 1614 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-proprio-t-03.
           if tipo-t (03) not = 0
              display proprio-t (03) at 1650 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-terceiros-t-03.
           if tipo-t (03) not = 0
              display terceiros-t (03) at 1664 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-tipo-t-04.
           if tipo-t (04) not = 0
              display tipo-t (04) at 1710 with foreground-color 15 
                      background-color 04
              display desc-t (04) at 1714 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-proprio-t-04.
           if tipo-t (04) not = 0
              display proprio-t (04) at 1750 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-terceiros-t-04.
           if tipo-t (04) not = 0
              display terceiros-t (04) at 1764 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-tipo-t-05.
           if tipo-t (05) not = 0
              display tipo-t (05) at 1810 with foreground-color 15 
                      background-color 04
              display desc-t (05) at 1814 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-proprio-t-05.
           if tipo-t (04) not = 0
              display proprio-t (05) at 1850 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-terceiros-t-05.
           if tipo-t (04) not = 0
              display terceiros-t (05) at 1864 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-proprio-total.
           if tipo-t (01) not = 0
              move 0 to total-aux-01
              add proprio-t (01) proprio-t (02) proprio-t (03) 
                  proprio-t (04) proprio-t (05) to total-aux-01
              display total-aux-01 at 1950 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-terceiros-total.
           if tipo-t (01) not = 0
              move 0 to total-aux-01
              add terceiros-t (01) terceiros-t (02) terceiros-t (03) 
                  terceiros-t (04) terceiros-t (05) to total-aux-01
              display total-aux-01 at 1964 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-funcionarios.
           display funcionarios at 1434 with foreground-color 15 
                   background-color 04.

      *
       dsp-funcionarios-r.
           display funcionarios-r at 1632 with foreground-color 15 
                   background-color 04.
      *
       dsp-guias-a.
           display guias-a at 1335 with foreground-color 15 
                   background-color 04.
      *
       dsp-guias-c.
           display guias-c at 1435 with foreground-color 15 
                   background-color 04.
      *
       dsp-guias-total.
           move 0 to total-aux-01.
           add guias-a guias-c to total-aux-01.
           display total-aux-01 at 1534 with foreground-color 15 
                   background-color 04.
      *
       dsp-faturamento.
           display faturamento at 1846 with foreground-color 15 
                   background-color 04.
      *
       dsp-fat-resp.
           display fat-resp at 2053 with foreground-color 15 
                   background-color 04.
      *
       dsp-fat-valor.
           move fat-valor to valor-aux.
           if fat-resp = "A"
              display valor-aux at 1654 with foreground-color 15 
                      background-color 04
           end-if.
           if fat-resp = "C"
              display valor-aux at 1854 with foreground-color 15 
                      background-color 04
           end-if.
      *
       dsp-sug-01.
           display sug-01 at 1519 with foreground-color 15 
                   background-color 04.
      *
       dsp-sug-02.
           display sug-02 at 1619 with foreground-color 15 
                   background-color 04.
      *
       dsp-sug-03.
           display sug-03 at 1719 with foreground-color 15 
                   background-color 04.
      *
       dsp-sug-04.
           display sug-04 at 1819 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-codigo.
           display limpa at 1319 with foreground-color 15 
                   background-color 04.
      *
       lmp-empresa.
           display limpa at 1419 with foreground-color 15 
                   background-color 04.
      *
       lmp-estrangeiros.
           display limpa-01 at 1942 with foreground-color 15 
                   background-color 04.
      *
       lmp-pax-i.
           display limpa-06 at 1432 with foreground-color 15 
                   background-color 04.
      *
       lmp-pax-g.
           display limpa-06 at 1532 with foreground-color 15 
                   background-color 04.
      *
       lmp-am-norte.
           display limpa-06 at 1340 with foreground-color 15 
                   background-color 04.
      *
       lmp-am-central.
           display limpa-06 at 1440 with foreground-color 15 
                   background-color 04.
      *
       lmp-am-sul.
           display limpa-06 at 1540 with foreground-color 15 
                   background-color 04.
      *
       lmp-europa.
           display limpa-06 at 1640 with foreground-color 15 
                   background-color 04.
      *
       lmp-asia.
           display limpa-06 at 1740 with foreground-color 15 
                   background-color 04.
      *
       lmp-africa.
           display limpa-06 at 1840 with foreground-color 15 
                   background-color 04.
      *
       lmp-tipo-t-01.
           display limpa at 1410 with foreground-color 15 
                   background-color 04.
      *
       lmp-proprio-t-01.
           display limpa-03 at 1450 with foreground-color 15 
                   background-color 04.
      *
       lmp-terceiros-t-01.
           display limpa-03 at 1464 with foreground-color 15 
                   background-color 04.
      *
       lmp-tipo-t-02.
           display limpa at 1510 with foreground-color 15 
                   background-color 04.
      *
       lmp-proprio-t-02.
           display limpa-03 at 1550 with foreground-color 15 
                   background-color 04.
      *
       lmp-terceiros-t-02.
           display limpa-03 at 1564 with foreground-color 15 
                   background-color 04.
      *
       lmp-tipo-t-03.
           display limpa at 1610 with foreground-color 15 
                   background-color 04.
      *
       lmp-proprio-t-03.
           display limpa-03 at 1650 with foreground-color 15 
                   background-color 04.
      *
       lmp-terceiros-t-03.
           display limpa-03 at 1664 with foreground-color 15 
                   background-color 04.
      *
       lmp-tipo-t-04.
           display limpa at 1710 with foreground-color 15 
                   background-color 04.
      *
       lmp-proprio-t-04.
           display limpa-03 at 1750 with foreground-color 15 
                   background-color 04.
      *
       lmp-terceiros-t-04.
           display limpa-03 at 1764 with foreground-color 15 
                   background-color 04.
      *
       lmp-tipo-t-05.
           display limpa at 1810 with foreground-color 15 
                   background-color 04.
      *
       lmp-proprio-t-05.
           display limpa-03 at 1850 with foreground-color 15 
                   background-color 04.
      *
       lmp-terceiros-t-05.
           display limpa-03 at 1864 with foreground-color 15 
                   background-color 04.
      *
       lmp-proprio-total.
           display limpa-06 at 1950 with foreground-color 15 
                   background-color 04.
      *
       lmp-terceiros-total.
           display limpa-06 at 1964 with foreground-color 15 
                   background-color 04.
      *
       lmp-funcionarios.
           display limpa-06 at 1434 with foreground-color 15 
                   background-color 04.
      *
       lmp-funcionarios-r.
           display limpa-06 at 1632 with foreground-color 15 
                   background-color 04.
      *
       lmp-guias-a.
           display limpa-06 at 1335 with foreground-color 15 
                   background-color 04.
      *
       lmp-guias-c.
           display limpa-06 at 1435 with foreground-color 15 
                   background-color 04.
      *
       lmp-faturamento.
           display limpa-01 at 1846 with foreground-color 15 
                   background-color 04.
      *
       lmp-fat-resp .
           display limpa-01 at 2053 with foreground-color 15 
                   background-color 04.
      *
       lmp-fat-valor.
           display limpa-06 at 1654 with foreground-color 15 
                   background-color 04.
           display limpa-06 at 1854 with foreground-color 15 
                   background-color 04.
      *
       lmp-sug-01.
           display limpa at 1519 with foreground-color 15 
                   background-color 04.
      *
       lmp-sug-02.
           display limpa at 1619 with foreground-color 15 
                   background-color 04.
      *
       lmp-sug-03.
           display limpa at 1719 with foreground-color 15 
                   background-color 04.
      *
       lmp-sug-04.
           display limpa at 1819 with foreground-color 15 
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
           perform rot-open-at02.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
           move spaces to rotina-codigo-cod.
      *
       lab-inc-01.
           display tela-09.
           display tela-02.
           move rotina-codigo-cod to codigo.
           perform lmp-codigo thru lmp-empresa.
           perform acc-codigo.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform sec-consulta
              go to lab-inc-01
           end-if.
           if escape-key = 4
              perform rot-pesq-agencia
              go to lab-inc-01
           end-if.
           move codigo to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-01
           end-if.
           move txt to codigo at01-codigo at02-codigo.
           perform dsp-codigo.
           perform rot-le-at01.
           if erro not = 0
              perform err-codigo-n
              go to lab-inc-01
           end-if.
           perform rot-le-at02.
           if erro = 0
              perform err-codigo-c
              go to lab-inc-01
           end-if.
           move at01-empresa-a to empresa.
           perform dsp-empresa.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           if kbd-aux = 1
              go to lab-inc-01
           end-if.
      *
       lab-inc-02.
           perform sec-inc-01.
           if escape-key = 1
              move spaces to rotina-codigo-cod
              go to lab-inc-01
           end-if.
      *
       lab-inc-03.
           perform sec-inc-02.
           if escape-key = 1
              go to lab-inc-02
           end-if.
      *
       lab-inc-04.
           perform sec-inc-03.
           if escape-key = 1
              go to lab-inc-03
           end-if.
      *
       lab-inc-05.
           perform sec-inc-04.
           if escape-key = 1
              go to lab-inc-04
           end-if.
      *
       lab-inc-06.
           perform sec-inc-05.
           if escape-key = 1
              go to lab-inc-05
           end-if.
      *
       lab-inc-07.
           perform sec-inc-06.
           if escape-key = 1
              go to lab-inc-06
           end-if.
      *
       lab-inc-08.
           perform sec-inc-07.
           if escape-key = 1
              go to lab-inc-07
           end-if.
      *
       lab-inc-09.
           perform sec-inc-08.
           if escape-key = 1
              go to lab-inc-08
           end-if.
      *
       lab-inc-10.
           perform sec-inc-09.
           if escape-key = 1
              go to lab-inc-09
           end-if.
      *
       lab-inc-11.
           perform sec-inc-10.
           if escape-key = 1
              go to lab-inc-10
           end-if.
      *
       lab-inc-12.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad-10.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad-10
              go to lab-inc-11
           end-if.
           if resposta = "N"
              perform rot-remonta
              move spaces to rotina-codigo-cod
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-12
              end-if
           end-if.
      *
       lab-inc-13.
           perform rot-move-at02.
           write reg-at02 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQAT02A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           display tela-06.
           perform rot-keypress.
           perform rot-remonta.
           move spaces to rotina-codigo-cod.
           go to lab-inc-01.
      *
       lab-inc-fim.
           perform rot-close-at02.
           perform rot-close-at01.
           perform rot-close-tabl.
           exit.
      *
       sec-inc-01 section.
      *
       lab-inc-01-00.
           perform rot-display-tela-01.
      *
       lab-inc-01-01.
           move spaces to estrangeiros.
           perform lmp-estrangeiros.
           perform acc-estrangeiros.
           if escape-key = 1
              perform rot-restore-tela-01
              go to lab-inc-01-fim
           end-if.
           move estrangeiros to txt.
           perform rot-texto.
           if txt not = "A" and "B" and "C" and "D" and 
                        "E" and "F" and space
              go to lab-inc-01-01
           end-if.
           move txt to estrangeiros.
           perform dsp-estrangeiros.
      *
       lab-inc-01-fim.
           exit.
      *
       sec-inc-02 section.
      *
       lab-inc-02-00.
           perform rot-display-tela-02.
      *
       lab-inc-02-01.
           move 0 to pax-i pax-g.
           perform dsp-pax-total.
           perform lmp-pax-i.
           perform acc-pax-i.
           if escape-key = 1
              perform rot-restore-tela-02
              go to lab-inc-02-fim
           end-if.
           perform dsp-pax-i.
      *
       lab-inc-02-02.
           move 0 to pax-g.
           perform dsp-pax-total.
           perform lmp-pax-g.
           perform acc-pax-g.
           if escape-key = 1
              perform lmp-pax-g
              go to lab-inc-02-01
           end-if.
           perform dsp-pax-g thru dsp-pax-total.           
           if (pax-i + pax-g) not = 100 and 0
              go to lab-inc-02-02
           end-if.

      *
       lab-inc-02-fim.
           exit.
      *
       sec-inc-03 section.
      *
       lab-inc-03-00.
           perform rot-display-tela-03.
      *
       lab-inc-03-01.
           move 0 to am-norte am-central am-sul europa asia africa.
           perform dsp-mundo-total.
           perform lmp-am-norte.
           perform acc-am-norte.
           if escape-key = 1
              perform rot-restore-tela-03
              go to lab-inc-03-fim
           end-if.
           if am-norte > 100
              go to lab-inc-03-01
           end-if.
      *
       lab-inc-03-02.
           move 0 to am-central am-sul europa asia africa.
           perform dsp-mundo-total.
           perform lmp-am-central.
           perform acc-am-central.
           if escape-key = 1
              perform lmp-am-central
              go to lab-inc-03-01
           end-if.
           if (am-norte + am-central) > 100
              go to lab-inc-03-02
           end-if.
      *
       lab-inc-03-03.
           move 0 to am-sul europa asia africa.
           perform dsp-mundo-total.
           perform lmp-am-sul.
           perform acc-am-sul.
           if escape-key = 1
              perform lmp-am-sul
              go to lab-inc-03-02
           end-if.
           if (am-norte + am-central + am-sul) > 100
              go to lab-inc-03-03
           end-if.
      *
       lab-inc-03-04.
           move 0 to europa asia africa.
           perform dsp-mundo-total.
           perform lmp-europa.
           perform acc-europa.
           if escape-key = 1
              perform lmp-europa
              go to lab-inc-03-03
           end-if.
           if (am-norte + am-central + am-sul + europa) > 100
              go to lab-inc-03-04
           end-if.
      *
       lab-inc-03-05.
           move 0 to asia africa.
           perform dsp-mundo-total.
           perform lmp-asia.
           perform acc-asia.
           if escape-key = 1
              perform lmp-asia
              go to lab-inc-03-04
           end-if.
           if (am-norte + am-central + am-sul + europa + asia) > 100
              go to lab-inc-03-05
           end-if.
      *
       lab-inc-03-06.
           move 0 to africa.
           perform dsp-mundo-total.
           perform lmp-africa.
           perform acc-africa.
           if escape-key = 1
              perform lmp-africa
              go to lab-inc-03-05
           end-if.
           perform dsp-mundo-total.
           if (am-norte + am-central + am-sul + europa + asia + africa)
              not = 100 and 0
              go to lab-inc-03-06
           end-if.
      *
       lab-inc-03-fim.
           exit.
      *
       sec-inc-04 section.
      *
       lab-inc-04-00.
           perform rot-display-tela-04.
      *
       lab-inc-04-01.
           display tela-05.
           move 0 to tipo-t (01) proprio-t (01) terceiros-t (01)
                     tipo-t (02) proprio-t (02) terceiros-t (02)
                     tipo-t (03) proprio-t (03) terceiros-t (03)
                     tipo-t (04) proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-tipo-t-01.
           perform acc-tipo-t-01.
           move tipo-t (01)to tipo-t-aux.
           if escape-key = 1
              perform rot-restore-tela-04
              go to lab-inc-04-fim
           end-if.
           if escape-key = 3
              move 7 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-04-01
           end-if.
           if tipo-t (01) = 0
              perform lmp-tipo-t-01
              go to lab-inc-04-fim
           end-if.
           move 1 to sub-aux.
           perform rot-search.
           if erro = 0
              perform lmp-tipo-t-01
              go to lab-inc-04-01
           end-if.
           move 07 to wtab01-tipo.
           move tipo-t (01) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-veiculo-n
              go to lab-inc-04-01
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to desc-t (01).
           perform dsp-tipo-t-01.
           display tela-limpa-cad-04.
      *
       lab-inc-04-02.
           move 0 to proprio-t (01) terceiros-t (01)
                     tipo-t (02) proprio-t (02) terceiros-t (02)
                     tipo-t (03) proprio-t (03) terceiros-t (03)
                     tipo-t (04) proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-proprio-t-01.
           perform acc-proprio-t-01.
           if escape-key = 1
              perform lmp-proprio-t-01
              go to lab-inc-04-01
           end-if.
      *
       lab-inc-04-03.
           move 0 to terceiros-t (01)
                     tipo-t (02) proprio-t (02) terceiros-t (02)
                     tipo-t (03) proprio-t (03) terceiros-t (03)
                     tipo-t (04) proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-terceiros-t-01.
           perform acc-terceiros-t-01.
           if escape-key = 1
              perform lmp-terceiros-t-01
              go to lab-inc-04-02
           end-if.
           if (proprio-t (01) + terceiros-t (01)) = 0
              go to lab-inc-04-03
           end-if.
      *
       lab-inc-04-04.
           display tela-05.
           move 0 to tipo-t (02) proprio-t (02) terceiros-t (02)
                     tipo-t (03) proprio-t (03) terceiros-t (03)
                     tipo-t (04) proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-tipo-t-02.
           perform acc-tipo-t-02.
           move tipo-t (02) to tipo-t-aux.
           if escape-key = 1
              perform lmp-tipo-t-02
              go to lab-inc-04-03
           end-if.
           if escape-key = 3
              move 7 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-04-04
           end-if.
           if tipo-t (02) = 0
              perform lmp-tipo-t-02
              go to lab-inc-04-fim
           end-if.
           move 2 to sub-aux.
           perform rot-search.
           if erro = 0
              perform lmp-tipo-t-02
              go to lab-inc-04-04
           end-if.
           move 07 to wtab01-tipo.
           move tipo-t (02) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-veiculo-n
              go to lab-inc-04-04
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to desc-t (02).
           perform dsp-tipo-t-02.
           display tela-limpa-cad-04.
      *
       lab-inc-04-05.
           move 0 to proprio-t (02) terceiros-t (02)
                     tipo-t (03) proprio-t (03) terceiros-t (03)
                     tipo-t (04) proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-proprio-t-02.
           perform acc-proprio-t-02.
           if escape-key = 1
              perform lmp-proprio-t-02
              go to lab-inc-04-04
           end-if.
      *
       lab-inc-04-06.
           move 0 to terceiros-t (02)
                     tipo-t (03) proprio-t (03) terceiros-t (03)
                     tipo-t (04) proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-terceiros-t-02.
           perform acc-terceiros-t-02.
           if escape-key = 1
              perform lmp-terceiros-t-02
              go to lab-inc-04-05
           end-if.
           if (proprio-t (02) + terceiros-t (02)) = 0
              go to lab-inc-04-06
           end-if.
      *
       lab-inc-04-07.
           display tela-05.
           move 0 to tipo-t (03) proprio-t (03) terceiros-t (03)
                     tipo-t (04) proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-tipo-t-03.
           perform acc-tipo-t-03.
           move tipo-t (03) to tipo-t-aux.
           if escape-key = 1
              perform lmp-tipo-t-03
              go to lab-inc-04-06
           end-if.
           if escape-key = 3
              move 7 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-04-07
           end-if.
           if tipo-t (03) = 0
              perform lmp-tipo-t-03
              go to lab-inc-04-fim
           end-if.
           move 3 to sub-aux.
           perform rot-search.
           if erro = 0
              perform lmp-tipo-t-03
              go to lab-inc-04-07
           end-if.
           move 07 to wtab01-tipo.
           move tipo-t (03) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-veiculo-n
              go to lab-inc-04-07
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to desc-t (03).
           perform dsp-tipo-t-03.
           display tela-limpa-cad-04.
      *
       lab-inc-04-08.
           move 0 to proprio-t (03) terceiros-t (03)
                     tipo-t (04) proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-proprio-t-03.
           perform acc-proprio-t-03.
           if escape-key = 1
              perform lmp-proprio-t-03
              go to lab-inc-04-07
           end-if.
      *
       lab-inc-04-09.
           move 0 to terceiros-t (03)
                     tipo-t (04) proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-terceiros-t-03.
           perform acc-terceiros-t-03.
           if escape-key = 1
              perform lmp-terceiros-t-03
              go to lab-inc-04-08
           end-if.
           if (proprio-t (03) + terceiros-t (03)) = 0
              go to lab-inc-04-09
           end-if.
      *
       lab-inc-04-10.
           display tela-05.
           move 0 to tipo-t (04) proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-tipo-t-04.
           perform acc-tipo-t-04.
           move tipo-t (04) to tipo-t-aux.
           if escape-key = 1
              perform lmp-tipo-t-04
              go to lab-inc-04-09
           end-if.
           if escape-key = 3
              move 7 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-04-10
           end-if.
           if tipo-t (04) = 0
              perform lmp-tipo-t-04
              go to lab-inc-04-fim
           end-if.
           move 4 to sub-aux.
           perform rot-search.
           if erro = 0
              perform lmp-tipo-t-04
              go to lab-inc-04-10
           end-if.
           move 07 to wtab01-tipo.
           move tipo-t (04) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-veiculo-n
              go to lab-inc-04-10
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to desc-t (04).
           perform dsp-tipo-t-04.
           display tela-limpa-cad-04.
      *
       lab-inc-04-11.
           move 0 to proprio-t (04) terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-proprio-t-04.
           perform acc-proprio-t-04.
           if escape-key = 1
              perform lmp-proprio-t-04
              go to lab-inc-04-10
           end-if.
      *
       lab-inc-04-12.
           move 0 to terceiros-t (04)
                     tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-terceiros-t-04.
           perform acc-terceiros-t-04.
           if escape-key = 1
              perform lmp-terceiros-t-04
              go to lab-inc-04-11
           end-if.
           if (proprio-t (04) + terceiros-t (04)) = 0
              go to lab-inc-04-12
           end-if.
      *
       lab-inc-04-13.
           display tela-05.
           move 0 to tipo-t (05) proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-tipo-t-05 
           perform acc-tipo-t-05.
           move tipo-t (05) to tipo-t-aux.
           if escape-key = 1
              perform lmp-tipo-t-05
              go to lab-inc-04-12
           end-if.
           if escape-key = 3
              move 7 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-04-13
           end-if.
           if tipo-t (05) = 0
              perform lmp-tipo-t-05
              go to lab-inc-04-fim
           end-if.
           move 5 to sub-aux.
           perform rot-search.
           if erro = 0
              perform lmp-tipo-t-05
              go to lab-inc-04-13
           end-if.
           move 07 to wtab01-tipo.
           move tipo-t (05) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-veiculo-n
              go to lab-inc-04-13
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to desc-t (05).
           perform dsp-tipo-t-05.
           display tela-limpa-cad-04.
      *
       lab-inc-04-14.
           move 0 to proprio-t (05) terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-proprio-t-05.
           perform acc-proprio-t-05.
           if escape-key = 1
              perform lmp-proprio-t-05
              go to lab-inc-04-13
           end-if.
      *
       lab-inc-04-15.
           move 0 to terceiros-t (05).
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform lmp-terceiros-t-05.
           perform acc-terceiros-t-05.
           if escape-key = 1
              perform lmp-terceiros-t-05
              go to lab-inc-04-14
           end-if.
           if (proprio-t (05) + terceiros-t (05)) = 0
              go to lab-inc-04-15
           end-if.
           perform dsp-proprio-total thru dsp-terceiros-total.
      *
       lab-inc-04-fim.
           exit.
      *
       sec-inc-05 section.
      *
       lab-inc-05-00.
           perform rot-display-tela-05.
      *
       lab-inc-05-01.
           move 0 to funcionarios.
           perform lmp-funcionarios.
           perform acc-funcionarios.
           if escape-key = 1
              perform rot-restore-tela-05
              go to lab-inc-05-fim
           end-if.
      *
       lab-inc-05-fim.
           exit.
      *
       sec-inc-06 section.
      *
       lab-inc-06-00.
           perform rot-display-tela-06.
      *
       lab-inc-06-01.
           move 0 to funcionarios-r.
           perform lmp-funcionarios-r.
           perform acc-funcionarios-r.
           if escape-key = 1
              perform rot-restore-tela-06
              go to lab-inc-06-fim
           end-if.
           if funcionarios not = 0
              if funcionarios-r > funcionarios
                 go to lab-inc-06-01
              end-if
           end-if.
      *
       lab-inc-06-fim.
           exit.
      *
       sec-inc-07 section.
      *
       lab-inc-07-00.
           perform rot-display-tela-07.
      *
       lab-inc-07-01.
           move 0 to guias-a guias-c.
           perform dsp-guias-total.
           perform lmp-guias-a.
           perform acc-guias-a.
           if escape-key = 1
              perform rot-restore-tela-07
              go to lab-inc-07-fim
           end-if.
      *
       lab-inc-07-02.
           move 0 to guias-c.
           perform dsp-guias-total.
           perform lmp-guias-c.
           perform acc-guias-c.
           if escape-key = 1
              perform lmp-guias-c
              go to lab-inc-07-01
           end-if.
           perform dsp-guias-total.
      *
       lab-inc-07-fim.
           exit.
      *
       sec-inc-08 section.
      *
       lab-inc-08-00.
           perform rot-display-tela-08.
      *
       lab-inc-08-01.
           move spaces to faturamento.
           perform lmp-faturamento.
           perform acc-faturamento.
           if escape-key = 1
              perform rot-restore-tela-08
              go to lab-inc-08-fim
           end-if.
           move faturamento to txt.
           perform rot-texto.
           if txt not = "A" and "B" and "C" and space
              go to lab-inc-08-01
           end-if.
           move txt to faturamento.
           perform dsp-faturamento.      
      *
       lab-inc-08-fim.
           exit.
      *
       sec-inc-09 section.
      *
       lab-inc-09-00.
           perform rot-display-tela-09.
      *
       lab-inc-09-01.
           move spaces to fat-resp.
           move 0 to fat-valor.
           perform lmp-fat-resp.
           perform acc-fat-resp.
           if escape-key = 1
              perform rot-restore-tela-09
              go to lab-inc-09-fim
           end-if.
           move fat-resp to txt.
           perform rot-texto.
           if txt not = "A" and "B" and "C" and space
              go to lab-inc-09-01
           end-if.
           move txt to fat-resp.
           if txt = "B" or space
              go to lab-inc-09-fim
           end-if.
           perform dsp-fat-resp.      
      *
       lab-inc-09-02.
           move 0 to fat-valor.
           perform lmp-fat-valor.
           perform acc-fat-valor.
           if escape-key = 1
              perform lmp-fat-valor
              go to lab-inc-09-01
           end-if.
           if fat-valor = 0
              go to lab-inc-09-02
           end-if.
           perform dsp-fat-valor.
      *
       lab-inc-09-fim.
           exit.           
      *
       sec-inc-10 section.
      *
       lab-inc-01-00.
           perform rot-display-tela-10.
      *
       lab-inc-10-01.
           move spaces to sug-01 sug-02 sug-03 sug-04.
           perform lmp-sug-01.
           perform acc-sug-01.
           if escape-key = 1
              perform rot-restore-tela-10
              go to lab-inc-10-fim
           end-if.
           if sug-01 = spaces
              go to lab-inc-10-fim
           end-if.
      *
       lab-inc-10-02.
           move spaces to sug-02 sug-03 sug-04.
           perform lmp-sug-02.
           perform acc-sug-02.
           if escape-key = 1
              perform lmp-sug-02
              go to lab-inc-10-01
           end-if.
           if sug-02 = spaces
              go to lab-inc-10-fim
           end-if.
      *
       lab-inc-10-03.
           move spaces to sug-03 sug-04.
           perform lmp-sug-03.
           perform acc-sug-03.
           if escape-key = 1
              perform lmp-sug-03
              go to lab-inc-10-02
           end-if.
           if sug-03 = spaces
              go to lab-inc-10-fim
           end-if.
      *
       lab-inc-10-04.
           move spaces to sug-04.
           perform lmp-sug-04.
           perform acc-sug-04.
           if escape-key = 1
              perform lmp-sug-04
              go to lab-inc-10-03
           end-if.
      *
       lab-inc-10-fim.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           display tela-limpa-cad.
           display tela-10.
      *
       lab-cns-01.
           perform sec-consulta-codigo.
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
           display tela-04.
           move low-values to at02-chave.
           move codigo to at02-codigo.
      *
       lab-cns-codigo-00-a.
           start arqat02 key is not less at02-chave.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or at02-codigo = codigo
              perform rot-inic-arquivo
              start arqat02 key is not less at02-chave
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if at02-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqat02 key is less at02-chave.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-at02
              go to lab-cns-codigo-fim
           end-if.
           if at02-chave = high-values
              perform rot-fim-arquivo
              start arqat02 key is not less at02-chave
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
                    when kbd-aux = 62
                         if erro = 0
                            perform rot-display-tela
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-codigo-03
                    when kbd-aux = 73
                         go to lab-cns-codigo-01
                    when kbd-aux = 71
                         move low-values to at02-chave
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to at02-chave
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-codigo thru lmp-empresa.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           perform lmp-codigo thru lmp-empresa.
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
           perform rot-le-at02-lock.
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
           delete arqat02 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQAT02A.DAT - Tecle <Enter>
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
           unlock arqat02 record.
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
           perform rot-le-at02-lock.
           perform rot-display.
      *
       lab-alt-01.
           perform sec-alt-01.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
      *
       lab-alt-02.
           perform sec-alt-02.
           if escape-key = 1
              go to lab-alt-01
           end-if.
      *
       lab-alt-03.
           perform sec-alt-03.
           if escape-key = 1
              go to lab-alt-02
           end-if.
      *
       lab-alt-04.
           perform sec-alt-04.
           if escape-key = 1
              go to lab-alt-03
           end-if.
      *
       lab-alt-05.
           perform sec-alt-05.
           if escape-key = 1
              go to lab-alt-04
           end-if.
      *
       lab-alt-06.
           perform sec-alt-06.
           if escape-key = 1
              go to lab-alt-05
           end-if.
      *
       lab-alt-07.
           perform sec-alt-07.
           if escape-key = 1
              go to lab-alt-06
           end-if.
      *
       lab-alt-08.
           perform sec-alt-08.
           if escape-key = 1
              go to lab-alt-07
           end-if.
      *
       lab-alt-09.
           perform sec-alt-09.
           if escape-key = 1
              go to lab-alt-08
           end-if.
      *
       lab-alt-10.
           perform sec-alt-10.
           if escape-key = 1
              go to lab-alt-09
           end-if.
      *
       lab-alt-11.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad-10.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad-10
              go to lab-alt-10
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-11
              end-if
           end-if.
           perform rot-move-at02.
           rewrite reg-at02 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQAT02A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-alt-fim
           end-rewrite.
           move "Registro alterado - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad-10.
           perform rot-keypress.
           display tela-limpa-cad-10.
      *
       lab-alt-fim.
           unlock arqat02 record.
           perform rot-remonta.
           display tela-10.
           display tela-04.
           exit.
      *
       sec-alt-01 section.
      *
       lab-alt-01-00.
           perform rot-display-tela-01.
      *
       lab-alt-01-01.
           perform acc-estrangeiros.
           if escape-key = 1
              perform rot-restore-tela-01
              go to lab-alt-01-fim
           end-if.
           move estrangeiros to txt.
           perform rot-texto.
           if txt not = "A" and "B" and "C" and "D" and 
                        "E" and "F" and spaces
              go to lab-alt-01-01
           end-if.
           move txt to estrangeiros.
           perform dsp-estrangeiros.
      *
       lab-alt-01-fim.
           exit.
      *
       sec-alt-02 section.
      *
       lab-alt-02-00.
           perform rot-display-tela-02.
           perform dsp-pax-i thru dsp-pax-total.
      *
       lab-alt-02-01.
           perform dsp-pax-total.
           perform acc-pax-i.
           if escape-key = 1
              perform rot-restore-tela-02
              go to lab-alt-02-fim
           end-if.
           perform dsp-pax-i.
      *
       lab-alt-02-02.
           perform dsp-pax-total.
           perform acc-pax-g.
           if escape-key = 1
              perform dsp-pax-g
              go to lab-alt-02-01
           end-if.
           perform dsp-pax-g thru dsp-pax-total.           
           if (pax-i + pax-g) not = 100 and 0
              go to lab-alt-02-02
           end-if.
      *
       lab-alt-02-fim.
           exit.
      *
       sec-alt-03 section.
      *
       lab-alt-03-00.
           perform rot-display-tela-03.
           perform dsp-am-norte thru dsp-mundo-total.
      *
       lab-alt-03-01.
           perform dsp-mundo-total.
           perform acc-am-norte.
           if escape-key = 1
              perform rot-restore-tela-03
              go to lab-alt-03-fim
           end-if.
           if am-norte > 100
              go to lab-alt-03-01
           end-if.
      *
       lab-alt-03-02.
           perform dsp-mundo-total.
           perform acc-am-central.
           if escape-key = 1
              perform dsp-am-central
              go to lab-alt-03-01
           end-if.
           if (am-norte + am-central) > 100
              go to lab-alt-03-02
           end-if.
      *
       lab-alt-03-03.
           perform dsp-mundo-total.
           perform acc-am-sul.
           if escape-key = 1
              perform dsp-am-sul
              go to lab-alt-03-02
           end-if.
           if (am-norte + am-central + am-sul) > 100
              go to lab-alt-03-03
           end-if.
      *
       lab-alt-03-04.
           perform dsp-mundo-total.
           perform acc-europa.
           if escape-key = 1
              perform dsp-europa
              go to lab-alt-03-03
           end-if.
           if (am-norte + am-central + am-sul + europa) > 100
              go to lab-alt-03-04
           end-if.
      *
       lab-alt-03-05.
           perform dsp-mundo-total.
           perform acc-asia.
           if escape-key = 1
              perform dsp-asia
              go to lab-alt-03-04
           end-if.
           if (am-norte + am-central + am-sul + europa + asia) > 100
              go to lab-alt-03-05
           end-if.
      *
       lab-alt-03-06.
           perform dsp-mundo-total.
           perform acc-africa.
           if escape-key = 1
              perform dsp-africa
              go to lab-alt-03-05
           end-if.
           perform dsp-mundo-total.
           if (am-norte + am-central + am-sul + europa + asia + africa)
              not = 100 and 0
              go to lab-alt-03-06
           end-if.
      *
       lab-alt-03-fim.
           exit.
      *
       sec-alt-04 section.
      *
       lab-alt-04-00.
           perform rot-display-tela-04.
           perform dsp-tipo-t-01 thru dsp-terceiros-total.
      *
       lab-alt-04-01.
           display tela-05.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-tipo-t-01.
           move tipo-t (01)to tipo-t-aux.
           if escape-key = 1
              perform rot-restore-tela-04
              go to lab-alt-04-fim
           end-if.
           if escape-key = 3
              move 7 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-04-01
           end-if.
           if tipo-t (01) = 0
              move 0 to proprio-t (01) terceiros-t (01)
                        tipo-t (02) proprio-t (02) terceiros-t (02)
                        tipo-t (03) proprio-t (03) terceiros-t (03)
                        tipo-t (04) proprio-t (04) terceiros-t (04)
                        tipo-t (05) proprio-t (05) terceiros-t (05)
              perform lmp-tipo-t-01 thru lmp-terceiros-t-05
              perform lmp-proprio-total thru lmp-terceiros-total
              go to lab-alt-04-fim
           end-if.
           move 1 to sub-aux.
           perform rot-search.
           if erro = 0
              go to lab-alt-04-01
           end-if.
           move 07 to wtab01-tipo.
           move tipo-t (01) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-veiculo-n
              go to lab-alt-04-01
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to desc-t (01).
           perform dsp-tipo-t-01.
           display tela-limpa-cad-04.
      *
       lab-alt-04-02.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-proprio-t-01.
           if escape-key = 1
              perform dsp-proprio-t-01
              go to lab-alt-04-01
           end-if.
      *
       lab-alt-04-03.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-terceiros-t-01.
           if escape-key = 1
              perform dsp-terceiros-t-01
              go to lab-alt-04-02
           end-if.
           if (proprio-t (01) + terceiros-t (01)) = 0
              go to lab-alt-04-03
           end-if.
      *
       lab-alt-04-04.
           display tela-05.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-tipo-t-02.
           move tipo-t (02) to tipo-t-aux.
           if escape-key = 1
              perform dsp-tipo-t-02
              go to lab-alt-04-03
           end-if.
           if escape-key = 3
              move 7 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-04-04
           end-if.
           if tipo-t (02) = 0
              move 0 to tipo-t (02) proprio-t (02) terceiros-t (02)
                        tipo-t (03) proprio-t (03) terceiros-t (03)
                        tipo-t (04) proprio-t (04) terceiros-t (04)
                        tipo-t (05) proprio-t (05) terceiros-t (05)
              perform lmp-tipo-t-01 thru lmp-terceiros-t-05
              perform dsp-proprio-total thru dsp-terceiros-total
              go to lab-alt-04-fim
           end-if.
           move 2 to sub-aux.
           perform rot-search.
           if erro = 0
              go to lab-alt-04-04
           end-if.
           move 07 to wtab01-tipo.
           move tipo-t (02) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-veiculo-n
              go to lab-alt-04-04
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to desc-t (02).
           perform dsp-tipo-t-02.
           display tela-limpa-cad-04.
      *
       lab-alt-04-05.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-proprio-t-02.
           if escape-key = 1
              perform dsp-proprio-t-02
              go to lab-alt-04-04
           end-if.
      *
       lab-alt-04-06.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-terceiros-t-02.
           if escape-key = 1
              perform dsp-terceiros-t-02
              go to lab-alt-04-05
           end-if.
           if (proprio-t (02) + terceiros-t (02)) = 0
              go to lab-alt-04-06
           end-if.
      *
       lab-alt-04-07.
           display tela-05.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-tipo-t-03.
           move tipo-t (03) to tipo-t-aux.
           if escape-key = 1
              perform dsp-tipo-t-03
              go to lab-alt-04-06
           end-if.
           if escape-key = 3
              move 7 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-04-07
           end-if.
           if tipo-t (03) = 0
              move 0 to tipo-t (03) proprio-t (03) terceiros-t (03)
                        tipo-t (04) proprio-t (04) terceiros-t (04)
                        tipo-t (05) proprio-t (05) terceiros-t (05)
              perform lmp-tipo-t-01 thru lmp-terceiros-t-05
              perform dsp-proprio-total thru dsp-terceiros-total
              go to lab-alt-04-fim
           end-if.
           move 3 to sub-aux.
           perform rot-search.
           if erro = 0
              go to lab-alt-04-07
           end-if.
           move 07 to wtab01-tipo.
           move tipo-t (03) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-veiculo-n
              go to lab-alt-04-07
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to desc-t (03).
           perform dsp-tipo-t-03.
           display tela-limpa-cad-04.
      *
       lab-alt-04-08.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-proprio-t-03.
           if escape-key = 1
              go to lab-alt-04-07
           end-if.
      *
       lab-alt-04-09.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-terceiros-t-03.
           if escape-key = 1
              perform dsp-terceiros-t-03
              go to lab-alt-04-08
           end-if.
           if (proprio-t (03) + terceiros-t (03)) = 0
              go to lab-alt-04-09
           end-if.
      *
       lab-alt-04-10.
           display tela-05.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-tipo-t-04.
           move tipo-t (04) to tipo-t-aux.
           if escape-key = 1
              perform dsp-tipo-t-04
              go to lab-alt-04-09
           end-if.
           if escape-key = 3
              move 7 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-04-10
           end-if.
           if tipo-t (04) = 0
              move 0 to tipo-t (04) proprio-t (04) terceiros-t (04)
                        tipo-t (05) proprio-t (05) terceiros-t (05)
              perform lmp-tipo-t-01 thru lmp-terceiros-t-05
              perform dsp-proprio-total thru dsp-terceiros-total
              go to lab-alt-04-fim
           end-if.
           move 4 to sub-aux.
           perform rot-search.
           if erro = 0
              go to lab-alt-04-10
           end-if.
           move 07 to wtab01-tipo.
           move tipo-t (04) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-veiculo-n
              go to lab-alt-04-10
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to desc-t (04).
           perform dsp-tipo-t-04.
           display tela-limpa-cad-04.
      *
       lab-alt-04-11.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-proprio-t-04.
           if escape-key = 1
              perform dsp-proprio-t-04
              go to lab-alt-04-10
           end-if.
      *
       lab-alt-04-12.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-terceiros-t-04.
           if escape-key = 1
              perform dsp-terceiros-t-04
              go to lab-alt-04-11
           end-if.
           if (proprio-t (04) + terceiros-t (04)) = 0
              go to lab-alt-04-12
           end-if.
      *
       lab-alt-04-13.
           display tela-05.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-tipo-t-05.
           move tipo-t (05) to tipo-t-aux.
           if escape-key = 1
              perform dsp-tipo-t-05
              go to lab-alt-04-12
           end-if.
           if escape-key = 3
              move 7 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-04-13
           end-if.
           if tipo-t (05) = 0
              move 0 to tipo-t (05) proprio-t (05) terceiros-t (05)
              perform lmp-tipo-t-01 thru lmp-terceiros-t-05
              perform dsp-proprio-total thru dsp-terceiros-total
              go to lab-alt-04-fim
           end-if.
           move 5 to sub-aux.
           perform rot-search.
           if erro = 0
              go to lab-alt-04-13
           end-if.
           move 07 to wtab01-tipo.
           move tipo-t (05) to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-veiculo-n
              go to lab-alt-04-13
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to desc-t (05).
           perform dsp-tipo-t-05.
           display tela-limpa-cad-04.
      *
       lab-alt-04-14.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-proprio-t-05.
           if escape-key = 1
              perform dsp-proprio-t-05
              go to lab-alt-04-13
           end-if.
      *
       lab-alt-04-15.
           perform dsp-proprio-total thru dsp-terceiros-total.
           perform acc-terceiros-t-05.
           if escape-key = 1
              perform dsp-terceiros-t-05
              go to lab-alt-04-14
           end-if.
           if (proprio-t (05) + terceiros-t (05)) = 0
              go to lab-alt-04-15
           end-if.
           perform dsp-proprio-total thru dsp-terceiros-total.
      *
       lab-alt-04-fim.
           exit.
      *
       sec-alt-05 section.
      *
       lab-alt-05-00.
           perform rot-display-tela-05.
      *     perform dsp-funcionarios.
      *
       lab-alt-05-01.
           perform acc-funcionarios.
           if escape-key = 1
              perform rot-restore-tela-05
              go to lab-alt-05-fim
           end-if.
      *
       lab-alt-05-fim.
           exit.
      *
       sec-alt-06 section.
      *
       lab-alt-06-00.
           perform rot-display-tela-06.
           perform dsp-funcionarios-r.
      *
       lab-alt-06-01.
           perform acc-funcionarios-r.
           if escape-key = 1
              perform rot-restore-tela-06
              go to lab-alt-06-fim
           end-if.
           if funcionarios not = 0
              if funcionarios-r > funcionarios
                 go to lab-alt-06-01
              end-if
           end-if.
      *
       lab-alt-06-fim.
           exit.
      *
       sec-alt-07 section.
      *
       lab-alt-07-00.
           perform rot-display-tela-07.
           perform dsp-guias-a thru dsp-guias-total.
      *
       lab-alt-07-01.
           perform dsp-guias-total.
           perform acc-guias-a.
           if escape-key = 1
              perform rot-restore-tela-07
              go to lab-alt-07-fim
           end-if.
      *
       lab-alt-07-02.
           perform dsp-guias-total.
           perform acc-guias-c.
           if escape-key = 1
              perform dsp-guias-c
              go to lab-alt-07-01
           end-if.
           perform dsp-guias-total.
      *
       lab-alt-07-fim.
           exit.
      *
       sec-alt-08 section.
      *
       lab-alt-08-00.
           perform rot-display-tela-08.
           perform dsp-faturamento.
      *
       lab-alt-08-01.
           perform acc-faturamento.
           if escape-key = 1
              perform rot-restore-tela-08
              go to lab-alt-08-fim
           end-if.
           move faturamento to txt.
           perform rot-texto.
           if txt not = "A" and "B" and "C" and space
              go to lab-alt-08-01
           end-if.
           move txt to faturamento.
           perform dsp-faturamento.      
      *
       lab-alt-08-fim.
           exit.
      *
       sec-alt-09 section.
      *
       lab-alt-09-00.
           perform rot-display-tela-09 .
           perform dsp-fat-resp thru dsp-fat-valor.
      *
       lab-alt-09-01.
           perform acc-fat-resp.
           if escape-key = 1
              perform rot-restore-tela-09
              go to lab-alt-09-fim
           end-if.
           move fat-resp to txt.
           perform rot-texto.
           if txt not = "A" and "B" and "C" and space
              go to lab-alt-09-01
           end-if.
           move txt to fat-resp.
           if txt = "B" or space
              move 0 to fat-valor
              perform lmp-fat-valor
              go to lab-alt-09-fim
           end-if.
           perform dsp-fat-resp.
           perform lmp-fat-valor.
      *
       lab-alt-09-02.
           perform acc-fat-valor.
           if escape-key = 1
              perform dsp-fat-valor
              go to lab-alt-09-01
           end-if.
           if fat-valor = 0
              go to lab-alt-09-02
           end-if.
           perform dsp-fat-valor.
      *
       lab-alt-09-fim.
           exit.           
      *
       sec-alt-10 section.
      *
       lab-alt-01-00.
           perform rot-display-tela-10.
           perform dsp-sug-01 thru dsp-sug-04.
      *
       lab-alt-10-01.
           perform acc-sug-01.
           if escape-key = 1
              perform rot-restore-tela-10
              go to lab-alt-10-fim
           end-if.
           if sug-01 = spaces
              go to lab-alt-10-fim
           end-if.
      *
       lab-alt-10-02.
           perform acc-sug-02.
           if escape-key = 1
              go to lab-alt-10-01
           end-if.
           if sug-02 = spaces
              go to lab-alt-10-fim
           end-if.
      *
       lab-alt-10-03.
           perform acc-sug-03.
           if escape-key = 1
              go to lab-alt-10-02
           end-if.
           if sug-03 = spaces
              go to lab-alt-10-fim
           end-if.
      *
       lab-alt-10-04.
           perform acc-sug-04.
           if escape-key = 1
              go to lab-alt-10-03
           end-if.
      *
       lab-alt-10-fim.
           exit.
      *
