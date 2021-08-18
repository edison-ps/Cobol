      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGPR02       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao da Pesquisa de Receptivo :                         *
      *                                                             *
      *  Data da ultima alteracao:    05/12/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgpr02.
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
       copy fdat02.lib.
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
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGPR02".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(24) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 sub                          pic 9(01) value 0.
       01 pagina                       pic 9(03) value 0.
       01 total                        pic 9(04) value 0.
       01 tracos-i                     pic x(80) value all "-".
      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
          02 sele-uf                   pic x(02) value spaces.
          02 sele-uf-disp              pic x(05) value spaces.
          02 sele-desc-t               pic x(30) occurs 5.
      *
       01 fat-aux.
          02 fat-texto                 pic x(16) value spaces.
          02 filler                    pic x(01) value spaces.
          02 fat-valor                 pic zz9,99 value 0.
          02 filler                    pic x(01) value spaces.
          02 fat-porcento              pic x(01) value "%".
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
             "Relacao da Pesquisa de Receptivo".
          02 filler                    pic x(17) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-01.
          02 filler                    pic x(05) value "U.F.:".
          02 filler                    pic x(01) value spaces.
          02 cab-estado                pic x(20) value spaces.
      *
       01 cab-02.
          02 filler                    pic x(10) value "Codigo...:".
          02 filler                    pic x(01) value spaces.
          02 cab-codigo                pic x(06) value spaces.
      *
       01 cab-03.
          02 filler                    pic x(10) value "Empresa..:".
          02 filler                    pic x(01) value spaces.
          02 cab-empresa               pic x(40) value spaces.
      *
       01 cab-04.
          02 filler                    pic x(53) value
             "1) Numero anual aproximado de turistas estrangeiros :".
          02 filler                    pic x(01) value spaces.
          02 cab-estrangeiros          pic x(24) value spaces.
      *
       01 cab-05.
          02 filler                    pic x(24) value 
             "2) Tipo de passageiros :".
      *
       01 cab-06.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(12) value "Individual :".
          02 filler                    pic x(01) value spaces.
          02 cab-pax-i                 pic zz9,99 value 0.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "%".
      *
       01 cab-07.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(07) value "Grupo :".
          02 filler                    pic x(06) value spaces.
          02 cab-pax-g                 pic zz9,99 value 0.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "%".
      *
       01 cab-08.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(07) value "Total :".
          02 filler                    pic x(06) value spaces.
          02 filler                    pic zz9,99 value 100.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(01) value "%".
      *
       01 cab-09.
          02 filler                    pic x(45) value 
          "3) Divisao provavel por area de procedencia :".
      *
       01 cab-10.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(25) value 
             "Estados Unidos / Canada :".
          02 filler                    pic x(01) value spaces.
          02 cab-am-norte              pic zz9,99 value 0.
          02 filler                    pic x(02) value " %".
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(26) value
             "Mexico / America Central :".
          02 filler                    pic x(01) value spaces.
          02 cab-am-central            pic zz9,99 value 0.
          02 filler                    pic x(02) value " %".
      *
       01 cab-11.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(16) value 
             "America do Sul :".
          02 filler                    pic x(10) value spaces.
          02 cab-am-sul                pic zz9,99 value 0.
          02 filler                    pic x(02) value " %".
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(08) value "Europa :".
          02 filler                    pic x(19) value spaces.
          02 cab-europa                pic zz9,99 value 0.
          02 filler                    pic x(02) value " %".
      *
       01 cab-12.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(24) value 
             "Asia e Extremo Oriente :".
          02 filler                    pic x(02) value spaces.
          02 cab-asia                  pic zz9,99 value 0.
          02 filler                    pic x(02) value " %".
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(08) value "Africa :".
          02 filler                    pic x(18) value spaces.
          02 cab-africa                pic zz9,99 value 0.
          02 filler                    pic x(02) value " %".
      *
       01 cab-13.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(07) value "Total :".
          02 filler                    pic x(19) value spaces.
          02 filler                    pic zz9,99 value 100.
          02 filler                    pic x(02) value " %".
      *
       01 cab-14.
          02 filler                    pic x(25) value 
             "4) Tipos de transportes :".
      *
       01 cab-15.
          02 filler                    pic x(11) value spaces.
          02 filler                    pic x(04) value "TIPO".
          02 filler                    pic x(35) value spaces.
          02 filler                    pic x(10) value "QUANTIDADE".
      *
       01 cab-16.
          02 filler                    pic x(45) value spaces.
          02 filler                    pic x(07) value "PROPRIO".
          02 filler                    pic x(04) value spaces.
          02 filler                    pic x(10) value "TERCEIROS".
     *
       01 cab-17.
          02 filler                    pic x(03) value spaces.
          02 cab-tipo-t                pic 9(03) value 0.
          02 filler                    pic x(01) value spaces.
          02 cab-desc-t                pic x(30) value spaces.
          02 filler                    pic x(12) value spaces.
          02 cab-proprio-t             pic 9(03) value 0.
          02 filler                    pic x(10) value spaces.
          02 cab-terceiros-t           pic 9(03) value 0.
      *
       01 cab-18.
          02 filler                    pic x(07) value spaces.
          02 filler                    pic x(07) value "TOTAL :".
          02 filler                    pic x(34) value spaces.
          02 cab-total-prop            pic 9(04) value 0.
          02 filler                    pic x(09) value spaces.
          02 cab-total-terc            pic 9(04) value 0.
      *
       01 cab-19.
          02 filler                    pic x(42) value 
             "5) Funcionarios contratados pela Agencia :".
          02 filler                    pic x(01) value spaces.
          02 cab-funcionarios          pic 9(03) value 0.
      *
       01 cab-20.
          02 filler                    pic x(58) value 
          "6) Funcionarios envolvidos exclusivamente com receptivo :".
          02 filler                    pic x(01) value spaces.
          02 cab-funcionarios-r        pic 9(03) value 0.
      *
       01 cab-21.
          02 filler                    pic x(44) value 
            "7) Numero de guias utilizados pela Agencia :".
          02 filler                    pic x(01) value spaces.
          02 cab-total-guias           pic 9(04) value 0.
      *
       01 cab-22.
          02 filler                    pic x(13) value spaces.
          02 filler                    pic x(11) value "Autonomos :".
          02 filler                    pic x(01) value spaces.
          02 cab-guias-a               pic 9(03) value 0.
          02 filler                    pic x(10) value spaces.
          02 filler                    pic x(13) value "Contratados :".
          02 filler                    pic x(01) value spaces.
          02 cab-guias-c               pic 9(03) value 0.
      *
       01 cab-23.
          02 filler                    pic x(55) value
             "8) Com a implantacao do Real o faturamento de Agencia :".
          02 filler                    pic x(01) value spaces.
          02 cab-faturamento           pic x(21) value spaces.
      *
       01 cab-24.
          02 filler                    pic x(53) value
             "9) Expectativa quanto ao faturamento no proximo ano :".
          02 filler                    pic x(01) value spaces.
          02 cab-fat-resp              pic x(25) value spaces.
      *
       01 cab-25.
          02 filler                    pic x(15) value
             "10) Sugestoes :".
      *
       01 cab-26.
          02 filler                    pic x(16) value spaces.
          02 cab-sugestao              pic x(40) value spaces.
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
          02 line 13 column 41 foreground-color 07 background-color 04
             highlight value "Relacao".
          02 line 14 column 08 foreground-color 06 background-color 04
             value "Ordenamento...:".
          02 line 15 column 08 foreground-color 06 background-color 04
             value "U.F...........:".
      *
       01 tela-02.
          02 line 17 column 07 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 17 column 10 foreground-color 07 background-color 02
             highlight value "1".
          02 line 17 column 11 foreground-color 01 background-color 02
             value "-Codigo".
          02 line 17 column 26 foreground-color 07 background-color 02
             highlight value "2".
          02 line 17 column 27 foreground-color 01 background-color 02
             value "-Empresa".
      *
       01 tela-03.
          02 line 17 column 07 foreground-color 07 background-color 02
             highlight pic x(41) from spaces.
          02 line 17 column 08 foreground-color 07 background-color 02
             highlight value "F2".
          02 line 17 column 10 foreground-color 01 background-color 02
             value " - Estados".
      *
       01 tela-08.
          02 line 17 column 07 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 17 column 07 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 17 column 14 foreground-color 07 background-color 02
             highlight value "C".
          02 line 17 column 15 foreground-color 01 background-color 02
             value ")ontinuar   (".
          02 line 17 column 28 foreground-color 07 background-color 02
             highlight value "F".
          02 line 17 column 29 foreground-color 01 background-color 02
             value ")inalizar".
      *
       01 tela-09.
          02 line 17 column 07 foreground-color 01 background-color 02
             pic x(41) from spaces.
          02 line 17 column 07 foreground-color 01 background-color 02
             value "Tecle (".
          02 line 17 column 14 foreground-color 07 background-color 02
             highlight value "I".
          02 line 17 column 15 foreground-color 01 background-color 02
             value ") para interromper".
      *
       01 tela-mensagem-cad.
          02 line 17 column 07 foreground-color 07 background-color 02
             highlight pic x(41) from mensagem.
      *
       01 tela-erro-cad.
          02 line 17 column 07 beep reverse-video pic x(41) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 17 column 07 foreground-color 04 background-color 04
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
           move 05 to box-col.
           move 11 to box-lin.
           move 47 to box-col-f.
           move 17 to box-lin-f.
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
       rot-imprime.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
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
           if sele-ord = 1
              move at02-codigo to cab-codigo
           else
              move at01-codigo to cab-codigo
           end-if.
           write reg-imp from cab-02 after 1 line.
           move at01-empresa-a to cab-empresa.
           write reg-imp from cab-03 after 1 line.
           write reg-imp from tracos-i after 1 line.

           evaluate true 
                    when at02-estrangeiros = "A"
                         move "ate 1.000" to cab-estrangeiros
                    when at02-estrangeiros = "B"
                         move "1.001 a 3.000" to cab-estrangeiros
                    when at02-estrangeiros = "C"
                         move "3.001 a 5.000" to cab-estrangeiros
                    when at02-estrangeiros = "D"
                         move "5.001 a 10.000" to cab-estrangeiros
                    when at02-estrangeiros = "E"
                         move "10.001 a 15.000" to cab-estrangeiros
                    when at02-estrangeiros = "F"
                         move "mais de 15.000" to cab-estrangeiros
           end-evaluate.
           write reg-imp from cab-04 after 2 line.

           move at02-pax-i to cab-pax-i.
           write reg-imp from cab-05 after 2 line
           write reg-imp from cab-06 after 2 line.

           move at02-pax-g to cab-pax-g.
           write reg-imp from cab-07 after 1 line.
           write reg-imp from cab-08 after 1 line
           

           move at02-am-norte to cab-am-norte.
           move at02-am-central to cab-am-central.
           write reg-imp from cab-09 after 2 line.
           write reg-imp from cab-10 after 2 line.

           move at02-am-sul to cab-am-sul.
           move at02-europa to cab-europa.
           write reg-imp from cab-11 after 1 line.

           move at02-asia to cab-asia.
           move at02-africa to cab-africa.
           write reg-imp from cab-12 after 1 line.
           write reg-imp from cab-13 after 1 line.
           write reg-imp from cab-14 after 2 line.
           write reg-imp from cab-15 after 2 line.
           write reg-imp from cab-16 after 1 line.
           write reg-imp from spaces after 1 line.
           move 1 to sub.
           move 0 to cab-total-prop cab-total-terc
           perform until sub = 6 or at02-tipo-t (sub) = 0
                   move at02-tipo-t (sub) to cab-tipo-t
                   move at02-proprio-t (sub) to cab-proprio-t
                   move at02-terceiros-t (sub) to cab-terceiros-t
                   add at02-proprio-t (sub) to cab-total-prop
                   add at02-terceiros-t (sub) to cab-total-terc

                   move 07 to wtab01-tipo
                   move at02-tipo-t (sub) to wtab01-codigo
                   move spaces to wtab01-resto
                   move wtab01-chave to tabl-chave
                   perform rot-le-tabl
                   if erro not = 0
                      move spaces to cab-desc-t
                   else
                      move reg-tabl to reg-wtab01
                      move wtab01-descricao to cab-desc-t
                   end-if

                   write reg-imp from cab-17 after 1 line
                   add 1 to sub
           end-perform.
           write reg-imp from cab-18 after 1 line.

           move at02-funcionarios to cab-funcionarios.
           write reg-imp from cab-19 after 2 line.

           move at02-funcionarios to cab-funcionarios-r.
           write reg-imp from cab-20 after 2 line.

           move at02-guias-a to cab-guias-a.
           move at02-guias-c to cab-guias-c.
           move 0 to cab-total-guias.
           add at02-guias-c at02-guias-a to cab-total-guias.
           write reg-imp from cab-21 after 2 line.
           write reg-imp from cab-22 after 2 line.

           evaluate true
                    when at02-faturamento = "A"
                         move "Diminuiu" to cab-faturamento
                    when at02-faturamento = "B"
                         move "Permaneceu igual" to cab-faturamento
                    when at02-faturamento = "C"
                         move "Aumentou" to cab-faturamento
           end-evaluate.
           write reg-imp from cab-23 after 2 line.

           evaluate true
                    when at02-fat-resp = "A"
                         move "Deve piorar em" to fat-texto
                         move at02-fat-valor to fat-valor
                         move "%" to fat-porcento
                         move fat-aux to cab-fat-resp
                    when at02-fat-resp = "B"
                         move "Deve permanecer igual" to cab-fat-resp
                    when at02-fat-resp = "C"
                         move "Deve piorar em" to fat-texto
                         move at02-fat-valor to fat-valor
                         move "%" to fat-porcento
                         move fat-aux to cab-fat-resp
           end-evaluate.
           write reg-imp from cab-24 after 2 line.

           move at02-sug-01 to cab-sugestao.
           write reg-imp from cab-25 after 2 line.
           write reg-imp from cab-26 after 2 line.

           move at02-sug-02 to cab-sugestao.
           write reg-imp from cab-26 after 1 line.

           move at02-sug-03 to cab-sugestao.
           write reg-imp from cab-26 after 1 line.

           move at02-sug-04 to cab-sugestao.
           write reg-imp from cab-26 after 1 line.
           add 1 to total.
      *
       rot-le-at01.
           move 0 to erro.
           read arqat01 invalid key move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-at01
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
       rot-le-proximo.
           move 0 to erro.
           evaluate true
                  when sele-ord = 1
                       read arqat02 next at end 
                            move 1 to erro
                       end-read
                       if at02-status = "9D"
                          move 0 to erro
                          call "C_Wait" using by value campo-wait
                          go to rot-le-proximo
                       end-if
                  when sele-ord = 2
                       read arqat01 next at end 
                            move 1 to erro
                       end-read
                       if at01-status = "9D"
                          move 0 to erro
                          call "C_Wait" using by value campo-wait
                          go to rot-le-proximo
                       end-if
           end-evaluate.
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
       rot-le-at02.
           move 0 to erro.
           read arqat02 invalid key move 1 to erro.
           if at02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-at02
           end-if.
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
           accept resposta at 1647 with auto foreground-color 04
                                             background-color 04.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
       err-estado.
            move " Estado nao cadastrado - Tecle <Enter>" to mensagem.
            display tela-erro-cad.
            perform rot-keypress.
            display tela-03.
      *
      *  Sequencia para dar Accept
      *
       acc-ord.
           accept sele-ord at 1424 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept sele-uf at 1524 with auto update prompt
                  foreground-color 15 background-color 04.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 1424 with foreground-color 15 
                   background-color 04.
      *
       dsp-uf.
           display sele-uf-disp at 1524 with foreground-color 15 
                   background-color 04.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1424 with foreground-color 15 
                   background-color 04.
      *
       lmp-uf.
           display limpa at 1524 with foreground-color 15 
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
           if sele-ord not = 1 and 2 
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
      *
       lab-sele-03.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-02
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-ord thru lmp-uf
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-03
              end-if
           end-if.
           display tela-limpa-cad.
           perform sec-impressao.
           perform lmp-ord thru lmp-uf.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform lmp-ord thru lmp-uf.
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
           perform rot-open-at02.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           move 0 to pagina total.
           evaluate true
                  when sele-ord = 1
                       move low-values to at02-chave
                       start arqat02 key is not less at02-chave
                  when sele-ord = 2
                       move low-values to at01-chave-1
                       start arqat01 key is not less at01-chave-1
           end-evaluate.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if at01-chave = high-values or at02-chave = high-values
              go to lab-imp-01
           end-if.
           perform rot-interrompe-impressora.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           evaluate true
                  when sele-ord = 1
                       move at02-codigo to at01-codigo
                       perform rot-le-at01
                  when sele-ord = 2
                       move at01-codigo to at02-codigo
                       perform rot-le-at02
                       if erro not = 0
                          go to lab-imp-01
                       end-if
           end-evaluate.
           if sele-uf not = spaces
              if at01-uf not = sele-uf
                 go to lab-imp-01
              end-if
           end-if.
           perform rot-imprime.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           if kbd2 not = 27 and pagina not = 0
              move total to cab-total
              write reg-imp from tracos-i after 2 line
              write reg-imp from cab-tot after 1 line
              write reg-imp from tracos-i after 1 line
           end-if.
           if pagina not = 0
              write reg-imp from spaces after page
           end-if.
           perform rot-close-imp.
           perform rot-close-at02.
           perform rot-close-at01.
           exit.
      *