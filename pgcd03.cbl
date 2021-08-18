      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGCD03       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao do Cadastro :                                      *
      *                                                             *
      *  Data da ultima alteracao:    26/05/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgcd03.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
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
           select arqcd02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is cd02-chave
                  alternate record key is cd02-chave-1 with duplicates
                  file status is cd02-status.
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
           select arqcab assign to disk
                  organization is line sequential
                  access mode is sequential
                  lock mode is manual
                  file status is cab-status.
      *
       data division.
       file section.
      *    
       copy fdcd01.lib.
      *    
       copy fdcd02.lib.
      *
       copy fdtabl.lib.
      *
       fd arqimp
      
       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *
       fd arqcab
      
       label record is standard
       value of file-id is nome-arq-cab
       data record is reg-cab.

       01 reg-cab                      pic x(50).
      *
       working-storage section.
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
       01 cd02-status                  pic x(02) value "00".
       01 cd02-stat                    pic x(01) value "F".
      *
       01 nome-arq-cd02.
          02 cd02-dir                  pic x(03) value "CD1".
          02 filler                    pic x(01) value "\".
          02 cd02-nome                 pic x(08) value "ARQCD02A".
          02 filler                    pic x(01) value ".".
          02 cd02-ext                  pic x(03) value "DAT".
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
       01 nome-arq-cab                 pic x(12) value "ARQCAB.IMP".
       01 cab-status                   pic x(02) value "00".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGCD03".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(48) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-empresa                 pic x(01) value "N".
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 linha                        pic 9(03) comp-5 value 0.
       01 pagina                       pic 9(03) value 0.
       01 tracos                       pic x(80) value all "-".
      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
          02 sele-tipo                 pic 9(01) value 0.
          02 sele-pessoa               pic x(01) value spaces.
          02 sele-pessoa-disp          pic x(05) value spaces.
          02 sele-uf                   pic x(02) value spaces.
          02 sele-uf-disp              pic x(05) value spaces.
          02 sele-categoria            pic 9(03) value 0.
          02 sele-dcategoria           pic x(40) value spaces.
          02 cgc-aux                   pic 99.999.999/9999b99.
          02 cpf-aux                   pic 999.999.999b99bbbb.
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
       01 cab-cliente.
          02 cliente                   pic x(40) value spaces.
      *
       01 cab-prog.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(19) value
          "Relacao do Cadastro".
          02 filler                    pic x(35) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-01.
          02 filler                    pic x(11) value "R.Social..:".
          02 filler                    pic x(01) value spaces.
          02 cab-razao-social          pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(10) value "Codigo...:".
          02 filler                    pic x(01) value spaces.
          02 cab-codigo                pic 9(05) value 0.
      *
       01 cab-02.
          02 filler                    pic x(11) value "N.Fantasia:".
          02 filler                    pic x(01) value spaces.
          02 cab-nome-fantasia         pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(10) value "Categoria:".
          02 filler                    pic x(01) value spaces.
          02 cab-categoria             pic 9(03) value 0.
      *
       01 cab-03.
          02 filler                    pic x(11) value "Contato...:".
          02 filler                    pic x(01) value spaces.
          02 cab-contato               pic x(40) value spaces.
      *
       01 cab-04.
          02 filler                    pic x(11) value "Endereco..:".
          02 filler                    pic x(01) value spaces.
          02 cab-endereco              pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(10) value "U.F......:".
          02 filler                    pic x(01) value spaces.
          02 cab-uf                    pic x(02) value spaces.
      *
       01 cab-05.
          02 filler                    pic x(11) value "Cidade....:".
          02 filler                    pic x(01) value spaces.
          02 cab-cidade                pic x(20) value spaces.
          02 filler                    pic x(21) value spaces.
          02 filler                    pic x(10) value "C.E.P....:".
          02 filler                    pic x(01) value spaces.
          02 cab-cep                   pic 99999b999 value 0.
      *
       01 cab-06.
          02 filler                    pic x(11) value "Fone......:".
          02 filler                    pic x(01) value spaces.
          02 cab-telefone              pic x(08) value spaces.
          02 filler                    pic x(33) value spaces.
          02 filler                    pic x(10) value "D.D.D....:".
          02 filler                    pic x(01) value spaces.
          02 cab-ddd                   pic 9(04) value 0.
      *
       01 cab-07.
          02 filler                    pic x(11) value "Fax.......:".
          02 filler                    pic x(01) value spaces.
          02 cab-fax                   pic x(08) value spaces.
          02 filler                    pic x(33) value spaces.
          02 filler                    pic x(10) value "Telex....:".
          02 filler                    pic x(01) value spaces.
          02 cab-telex                 pic x(08) value spaces.
      *
       01 cab-08.
          02 filler                    pic x(32) value
             "D a d o s  d e  C o b r a n c a".
      *
       01 cab-09.
          02 filler                    pic x(11) value "Endereco..:".
          02 filler                    pic x(01) value spaces.
          02 cab-endereco-c            pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(10) value "U.F......:".
          02 filler                    pic x(01) value spaces.
          02 cab-uf-c                  pic x(02) value spaces.
      *
       01 cab-10.
          02 filler                    pic x(11) value "Cidade....:".
          02 filler                    pic x(01) value spaces.
          02 cab-cidade-c              pic x(20) value spaces.
          02 filler                    pic x(21) value spaces.
          02 filler                    pic x(10) value "C.E.P....:".
          02 filler                    pic x(01) value spaces.
          02 cab-cep-c                 pic 99999b999 value 0.
      *
       01 cab-11.
          02 filler                    pic x(11) value "Cgc/Cpf...:".
          02 filler                    pic x(01) value spaces.
          02 cab-cgcpf                 pic x(18) value spaces.
          02 filler                    pic x(23) value spaces.
          02 filler                    pic x(10) value "I.E......:".
          02 filler                    pic x(01) value spaces.
          02 cab-ie                    pic x(10) value spaces.
      *
       01 cab-12.
          02 filler                    pic x(16) value 
             "C o n t a t o s".
      *
       01 cab-13.
          02 filler                    pic x(11) value "Contato...:".
          02 filler                    pic x(01) value spaces.
          02 cab-contato-c             pic x(40) value spaces.
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
          02 line 13 column 64 foreground-color 06 background-color 01
             highlight value "Relacao".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Ordenamento...:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Tipo..........:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Pessoa........:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "U.F...........:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "Categoria.....:".
      *
       01 tela-02.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 20 column 06 foreground-color 02 background-color 03
             highlight value "1".
          02 line 20 column 07 foreground-color 05 background-color 03
             value "-Codigo".
          02 line 20 column 16 foreground-color 02 background-color 03
             highlight value "2".
          02 line 20 column 17 foreground-color 05 background-color 03
             value "-Razao Social".
          02 line 20 column 32 foreground-color 02 background-color 03
             highlight value "3".
          02 line 20 column 33 foreground-color 05 background-color 03
             value "-Nome Fantasia".
          02 line 20 column 49 foreground-color 02 background-color 03
             highlight value "4".
          02 line 20 column 50 foreground-color 05 background-color 03
             value "-Categoria".
          02 line 20 column 62 foreground-color 02 background-color 03
             highlight value "5".
          02 line 20 column 63 foreground-color 05 background-color 03
             value "-Cgc/Cpf".
      *
       01 tela-04.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 20 column 06 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 20 column 08 foreground-color 05 background-color 03
             value " - Categorias".
      *
       01 tela-05.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 20 column 06 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 20 column 08 foreground-color 05 background-color 03
             value " - Estados".
      *
       01 tela-06.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 20 column 08 foreground-color 02 background-color 03
             highlight value "F".
          02 line 20 column 09 foreground-color 05 background-color 03
             value "-Fisica".
          02 line 20 column 25 foreground-color 02 background-color 03
             highlight value "J".
          02 line 20 column 26 foreground-color 05 background-color 03
             value "-Juridica".
      *
       01 tela-08.
          02 line 20 column 05 foreground-color 05 background-color 03
             pic x(66) from spaces.
          02 line 20 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 20 column 12 foreground-color 02 background-color 03
             highlight value "C".
          02 line 20 column 13 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 20 column 26 foreground-color 02 background-color 03
             highlight value "F".
          02 line 20 column 27 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-09.
          02 line 20 column 05 foreground-color 05 background-color 03
             pic x(66) from spaces.
          02 line 20 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 20 column 12 foreground-color 02 background-color 03
             highlight value "I".
          02 line 20 column 13 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-10.
          02 line 20 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 20 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 20 column 09 foreground-color 05 background-color 03
             value "-Sintetico".
          02 line 20 column 25 foreground-color 02 background-color 03
             highlight value "2".
          02 line 20 column 26 foreground-color 05 background-color 03
             value "-Analitico".
      *
       01 tela-mensagem-cad.
          02 line 20 column 05 foreground-color 07 background-color 01
             highlight pic x(66) from mensagem.
      *
       01 tela-erro-cad.
          02 line 20 column 05 beep reverse-video pic x(66) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 20 column 05 foreground-color 01 background-color 01
             pic x(66) from spaces.
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
           move 70 to box-col-f.
           move 20 to box-lin-f.
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
           perform rot-le-contato.
           move cd01-razao-social-a to cab-razao-social.
           move cd01-codigo to cab-codigo.
           move cd01-nome-fantasia-a to cab-nome-fantasia.
           move cd01-categoria to cab-categoria.
           move cd02-nome-a to cab-contato.
           move cd01-endereco to cab-endereco.
           move cd01-uf to cab-uf.
           move cd01-cidade to cab-cidade
           move cd01-cep to cab-cep.
           move cd01-telefone to cab-telefone.
           move cd01-ddd to cab-ddd.
           move cd01-fax to cab-fax.
           move cd01-telex to cab-telex.
           if sele-tipo = 2
              move cd01-endereco-cbr to cab-endereco-c
              move cd01-uf-cbr to cab-uf-c
              move cd01-cidade-cbr to cab-cidade-c
              move cd01-cep-cbr to cab-cep-c
              if cd01-flag-cgcpf = "F"
                 move cd01-cgcpf to cgc-aux
                 move cgc-aux to cab-cgcpf
              else
                 move cd01-cgcpf to cpf-aux
                 move cpf-aux to cab-cgcpf
              end-if
           move cd01-ie to cab-ie.
      *
       rot-move-cont.
           move cd02-nome-a to cab-contato-c.
      *
       rot-le-contato.
          move low-value to cd02-chave.
          move cd01-codigo to cd02-codigo.
          move 01 to cd02-contato.
          perform rot-le-cd02.
          if erro not = 0
             move spaces to cd02-nome-a
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
              write reg-imp from cab-cliente
              write reg-imp from cab-prog after 1 line
           else
              write reg-imp from cab-cliente after page
              write reg-imp from cab-prog after 2 lines
           end-if.
           write reg-imp from spaces after 1 line.
           write reg-imp from tracos after 1 line.
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
       err-leitura-cd01.
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
       rot-le-proximo.
           move 0 to erro.
           read arqcd01 next at end move 1 to erro.
           if cd01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-cd02.
           move 0 to erro.
           if cd02-stat = "F"
              open i-o arqcd02
              if cd02-status not = "00"
                 move 
                 " Erro de abertura no ARQCD02A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to cd02-stat
               end-if
           end-if.
      *
       rot-close-cd02.
           if cd02-stat = "A"
              close arqcd02
              move "F" to cd02-stat
           end-if.
      *
       err-leitura-cd02.
           move " Erro de leitura - ARQCD02A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-proximo-cd02.
           move 0 to erro.
           read arqcd02 next at end move 1 to erro.
           if cd02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo-cd02
           end-if.
      *
       rot-le-cd02.
           move 0 to erro.
           read arqcd02 invalid key move 1 to erro.
           if cd02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-cd02
           end-if.
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
              go to rot-le-tabl
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
       rot-pesq-tabela.
           perform rot-close-tabl.
           move 15 to rotina-col.
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
           move 12 to rotina-col-uf.
           move 10 to rotina-lin-uf.
           move "3" to rotina-borda-uf.
           move spaces to rotina-fundo-uf.
           move "S" to rotina-sombra-uf.
           call "pgtab02" using param-menu campo-rotina-uf.
           cancel "pgtab02".
           perform rot-open-tabl.
      *
       err-categoria.
           move " Categoria nao cadastrada - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
       err-estado.
           move " Estado nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
       rot-pesq-cab.
           move 0 to erro.
           open input arqcab.
           if cab-status = "00"
              read arqcab at end move 1 to erro
           else
              move 1 to erro
           end-if.
           close arqcab.
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
           accept resposta at 1940 with auto foreground-color 01
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
      *  Sequencia para dar Accept
      *
       acc-ord.
           accept sele-ord at 1422 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-tipo.
           accept sele-tipo at 1522 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-pessoa.
           accept sele-pessoa at 1622 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept sele-uf at 1722 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-categoria.
           accept sele-categoria at 1822 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 1422 with foreground-color 15 
                   background-color 01.
      *
       dsp-tipo.
           display sele-tipo at 1522 with foreground-color 15 
                   background-color 01.
      *
       dsp-pessoa.
           display sele-pessoa-disp at 1622 with foreground-color 15 
                   background-color 01.
      *
       dsp-uf.
           display sele-uf-disp at 1722 with foreground-color 15 
                   background-color 01.
      *
       dsp-categoria.
           display sele-categoria at 1822 with foreground-color 15 
                   background-color 01.
           display sele-dcategoria at 1826 with foreground-color 15 
                   background-color 01.
      *
       dsp-categoria-todas.
           display "Todas" at 1822 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1422 with foreground-color 15 
                   background-color 01.
      *
       lmp-tipo.
           display limpa at 1522 with foreground-color 15 
                   background-color 01.
      *
       lmp-pessoa.
           display limpa at 1622 with foreground-color 15 
                   background-color 01.
      *
       lmp-uf.
           display limpa at 1722 with foreground-color 15 
                   background-color 01.
      *
       lmp-categoria.
           display limpa at 1822 with foreground-color 15 
                   background-color 01.
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
           if sele-ord not = 1 and 2 and 3 and 4 and 5
              go to lab-sele-01
           end-if.
      *
       lab-sele-02.
           display tela-10.
           move 0 to sele-tipo.
           perform lmp-tipo.
           perform acc-tipo.
           if escape-key = 1
              perform lmp-tipo
              go to lab-sele-01
           end-if.
           if sele-tipo not = 1 and 2
              go to lab-sele-02
           end-if.
      *
       lab-sele-03.
           display tela-06.
           move spaces to sele-pessoa.
           perform lmp-pessoa.
           perform acc-pessoa.
           if escape-key = 1
              perform lmp-pessoa
              go to lab-sele-02
           end-if.
           move sele-pessoa to txt.
           perform rot-texto.
           if txt not = "F" and "J" and spaces
              go to lab-sele-03
           end-if.
           move txt to sele-pessoa sele-pessoa-disp.
           if sele-pessoa = spaces
              move "Todos" to sele-pessoa-disp
           end-if.
           perform dsp-pessoa.
           move spaces to rotina-uf.
      *
       lab-sele-04.
           display tela-05.
           move rotina-uf to sele-uf.
           perform lmp-uf.
           perform acc-uf.
           if escape-key = 1
              move 0 to rotina-codigo
              perform lmp-uf
              go to lab-sele-03
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-sele-04
           end-if.
           if sele-uf = spaces
              move "Todas" to sele-uf-disp
              perform dsp-uf
              display tela-limpa-cad
              go to lab-sele-05
           end-if.
           move sele-uf to txt.
           perform rot-texto.
           move txt to sele-uf sele-uf-disp.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move sele-uf to wtab03-sigla.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado
              display tela-limpa-cad
              go to lab-sele-04
           end-if.
           display tela-limpa-cad.
           move 0 to rotina-codigo.
      *
       lab-sele-05.
           display tela-04.
           move rotina-codigo to sele-categoria.
           move spaces to sele-dcategoria.
           perform lmp-categoria.
           perform acc-categoria.
           if escape-key = 1
              perform lmp-categoria
              move spaces to rotina-uf
              go to lab-sele-04
           end-if.
           if escape-key = 3
              move 1 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-sele-05
           end-if.
           if sele-categoria = 0
              perform dsp-categoria-todas
              go to lab-sele-06
           end-if.
           move 01 to wtab01-tipo.
           move sele-categoria to wtab01-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-categoria
              display tela-limpa-cad
              go to lab-sele-05
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to sele-dcategoria.
           perform dsp-categoria.
      *
       lab-sele-06.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-05
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-ord thru lmp-categoria
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-06
              end-if
           end-if.
           perform sec-impressao.
           display tela-limpa-cad.
           perform lmp-ord thru lmp-categoria.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform lmp-ord thru lmp-categoria.
           exit.
      *
       sec-impressao section.
      *
       lab-imp-00.
           perform rot-open-cd01.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-cd02.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-pesq-cab.
           if erro not = 0
              move spaces to cab-cliente
           else
              move reg-cab to cab-cliente
           end-if.
           move 99 to linha.
           move 0 to pagina.
           evaluate true
                  when sele-ord = 1
                       move low-values to cd01-chave
                       start arqcd01 key is not less cd01-chave
                  when sele-ord = 2
                       move low-values to cd01-chave-2
                       start arqcd01 key is not less cd01-chave-2
                  when sele-ord = 3
                       move low-values to cd01-chave-1
                       start arqcd01 key is not less cd01-chave-1
                  when sele-ord = 4
                       move low-values to cd01-chave-3
                       start arqcd01 key is not less cd01-chave-3
           end-evaluate.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if cd01-chave = high-values
              go to lab-imp-01
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if sele-pessoa not = spaces
              if cd01-flag-cgcpf not = sele-pessoa
                 go to lab-imp-01
              end-if
           end-if.
           if sele-uf not = spaces
              if cd01-uf not = sele-uf
                 go to lab-imp-01
              end-if
           end-if.
           if sele-categoria not = 0
              if cd01-categoria not = sele-categoria
                 go to lab-imp-01
              end-if
           end-if.
           if linha > 56
              perform rot-cabec
           end-if.
           perform rot-move.
           write reg-imp from cab-01 after 1 line.
           write reg-imp from cab-02 after 1 line.
           write reg-imp from cab-03 after 1 line.
           write reg-imp from cab-04 after 1 line.
           write reg-imp from cab-05 after 1 line.
           write reg-imp from cab-06 after 1 line.
           write reg-imp from cab-07 after 1 line.
           if sele-tipo = 2
              write reg-imp from tracos after 1 line
              write reg-imp from cab-08 after 1 line
              write reg-imp from tracos after 1 line
              write reg-imp from cab-09 after 1 line
              write reg-imp from cab-10 after 1 line
              write reg-imp from cab-11 after 1 line
              add 6 to linha
           end-if.
           write reg-imp from tracos after 1 line.
           add 8 to linha.
           if sele-tipo = 2
              write reg-imp from cab-12 after 1 line
              write reg-imp from tracos after 1 line
              perform sec-contato
              if resposta = "F"
                 move 27 to kbd2
                 go to lab-imp-fim
              end-if
           end-if.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           if kbd2 not = 27
              if sele-tipo = 1
                 write reg-imp from tracos after 1 line
              end-if
           end-if.
           if pagina not = 0
              write reg-imp from spaces after page
           end-if.
           perform rot-close-imp.
           perform rot-close-cd02.
           perform rot-close-cd01.
           exit.
      *
       sec-contato section.
      *
       lab-cont-00.
           move low-values to cd02-chave.
           move cd01-codigo to cd02-codigo.
           start arqcd02 key is not less cd02-chave.
      *
       lab-cont-01.
           perform rot-le-proximo-cd02.
           if erro not = 0
              go to lab-cont-fim
           end-if.
           if cd02-chave = high-values
              go to lab-cont-fim
           end-if.
           if cd02-codigo not = cd01-codigo 
              go to lab-cont-fim
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-cont-fim
           end-if.
           if linha > 56
              perform rot-cabec
           end-if.
           perform rot-move-cont.
           write reg-imp from cab-13 after 1 line.
           write reg-imp from tracos after 1 line.
           add 2 to linha.
           go to lab-cont-01.
      *
       lab-cont-fim.
           move 57 to linha.
           exit.