      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGCD02       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Emissao de etiquetas :                                     *
      *                                                             *
      *  Data da ultima alteracao:    02/10/93     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgcd02.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
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
           select arqimp assign to printer
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
           select arqsort assign to disk.
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
       sd arqsort.

       01 reg-sort.
          02 sort-cep                  pic 9(08).
          02 sort-codigo               pic 9(05).
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
       01 sort-status                  pic x(02) value "00".
       01 sort-stat                    pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGCD02".
          02 cb-versao                 pic x(06) value "v1.01 ".
      *
       01 limpa                        pic x(35) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 sub                          pic 9(02) value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 sele-codigo-i             pic 9(05) value 0.
          02 sele-dcodigo-i            pic x(28) value spaces.
          02 sele-codigo-i-disp        pic x(07) value spaces.
          02 sele-codigo-f             pic 9(05) value 0.
          02 sele-dcodigo-f            pic x(28) value spaces.
          02 sele-codigo-f-disp        pic x(05) value spaces.
          02 sele-uf                   pic x(02) value spaces.
          02 sele-cep-i                pic 9(08) value 0.
          02 sele-cep-f                pic 9(08) value 0.
          02 sele-cep-i-aux            pic 99999b999.
          02 sele-cep-f-aux            pic 99999b999.
          02 sele-cep-i-disp           pic x(09) value spaces.
          02 sele-cep-f-disp           pic x(09) value spaces.
          02 sele-categoria            pic 9(03) value 0.
          02 sele-dcategoria           pic x(30) value spaces.
          02 sele-endereco             pic x(01) value spaces.
          02 sele-remetente            pic x(01) value spaces.
          02 sele-contato              pic x(01) value spaces.
          02 sele-contato-disp         pic x(05) value spaces.
      *
       01 cab-01.
          02 filler                    pic x(35) value spaces.
          02 cab-codigo                pic 9(05) value 0.
          02 filler                    pic x(39) value spaces.
          02 cab-codigo-1              pic 9(05) value 0.
      *
       01 cab-02.
          02 cab-razao                 pic x(40) value spaces.
          02 filler                    pic x(04) value spaces.
          02 cab-razao-1               pic x(40) value spaces.
      *
       01 cab-02-a.
          02 cab-ac                    pic x(03) value spaces.
          02 filler                    pic x(01) value spaces.
          02 cab-contato               pic x(36) value spaces.
          02 filler                    pic x(04) value spaces.
          02 cab-ac-1                  pic x(03) value spaces.
          02 filler                    pic x(01) value spaces.
          02 cab-contato-1             pic x(36) value spaces.
      *
       01 cab-03.
          02 cab-endereco              pic x(40) value spaces.
          02 filler                    pic x(04) value spaces.
          02 cab-endereco-1            pic x(40) value spaces.
      *
       01 cab-04.
          02 cab-cep                   pic 9(05)b9(03) value 0.
          02 filler                    pic x(02) value spaces.
          02 cab-cidade                pic x(15) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-uf                    pic x(02) value spaces.
          02 filler                    pic x(14) value spaces.
          02 cab-cep-1                 pic 9(05)b9(03) value 0.
          02 filler                    pic x(02) value spaces.
          02 cab-cidade-1              pic x(15) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-uf-1                  pic x(02) value spaces.
      *
       01 campo-rotina-cod.
          02 rotina-col-cod            pic 9(02) value 0.
          02 rotina-lin-cod            pic 9(02) value 0.
          02 rotina-borda-cod          pic x(01) value spaces.
          02 rotina-fundo-cod          pic x(01) value spaces.
          02 rotina-sombra-cod         pic x(01) value spaces.
          02 rotina-codigo-cod         pic 9(05) value 0.
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
          02 line 10 column 61 foreground-color 06 background-color 01
             highlight value "Etiquetas".
          02 line 11 column 13 foreground-color 06 background-color 01
             highlight value "Codigo Inicial.....:".
          02 line 12 column 13 foreground-color 06 background-color 01
             highlight value "Codigo Final.......:".
          02 line 13 column 13 foreground-color 06 background-color 01
             highlight value "C.e.p. Inicial.....:".
          02 line 14 column 13 foreground-color 06 background-color 01
             highlight value "C.e.p. Final.......:".
          02 line 15 column 13 foreground-color 06 background-color 01
             highlight value "Categoria..........:".
          02 line 16 column 13 foreground-color 06 background-color 01
             highlight value "U.F................:".
          02 line 17 column 13 foreground-color 06 background-color 01
             highlight value "Endereco Cbr.......:".
          02 line 18 column 13 foreground-color 06 background-color 01
             highlight value "Remetente..........:".
          02 line 19 column 13 foreground-color 06 background-color 01
             highlight value "Contato............:".
      *
       01 tela-02.
          02 line 21 column 11 foreground-color 05 background-color 03
             highlight pic x(59) from spaces.
          02 line 21 column 11 foreground-color 05 background-color 03
             value "(".
          02 line 21 column 12 foreground-color 02 background-color 03
             highlight value "I".
          02 line 21 column 13 foreground-color 05 background-color 03
             value ")mprimir   (".
          02 line 21 column 25 foreground-color 02 background-color 03
             highlight value "P".
          02 line 21 column 26 foreground-color 05 background-color 03
             value ")osicionar   (".
          02 line 21 column 40 foreground-color 02 background-color 03
             highlight value "E".
          02 line 21 column 41 foreground-color 05 background-color 03
             value ")ncerrar".
      *
       01 tela-03.
          02 line 21 column 11 foreground-color 05 background-color 03
             pic x(59) from spaces.
          02 line 21 column 11 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 21 column 18 foreground-color 02 background-color 03
             highlight value "I".
          02 line 21 column 19 foreground-color 05 background-color 03
             value ")nterroper".
      *
       01 tela-04.
          02 line 21 column 11 foreground-color 05 background-color 03
             highlight pic x(59) from spaces.
          02 line 21 column 11 foreground-color 02 background-color 03
             highlight value " Arquivo em ordenamento...".
      *
       01 tela-05.
          02 line 21 column 11 foreground-color 02 background-color 03
             highlight pic x(59) from spaces.
          02 line 21 column 12 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 14 foreground-color 05 background-color 03
             value "-Cadastro".
      *
       01 tela-06.
          02 line 21 column 11 foreground-color 02 background-color 03
             highlight pic x(59) from spaces.
          02 line 21 column 12 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 14 foreground-color 05 background-color 03
             value "-Categorias".
      *
       01 tela-07.
          02 line 21 column 11 foreground-color 02 background-color 03
             highlight pic x(59) from spaces.
          02 line 21 column 12 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 14 foreground-color 05 background-color 03
             value "-Estados".
      *
       01 tela-08.
          02 line 21 column 11 foreground-color 05 background-color 03
             pic x(59) from spaces.
          02 line 21 column 11 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 21 column 18 foreground-color 02 background-color 03
             highlight value "C".
          02 line 21 column 19 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 21 column 32 foreground-color 02 background-color 03
             highlight value "F".
          02 line 21 column 33 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-09.
          02 line 21 column 11 foreground-color 02 background-color 03
             highlight pic x(59) from spaces.
          02 line 21 column 14 foreground-color 02 background-color 03
             highlight value "S".
          02 line 21 column 15 foreground-color 05 background-color 03
             value "-Sim".
          02 line 21 column 31 foreground-color 02 background-color 03
             highlight value "N".
          02 line 21 column 32 foreground-color 05 background-color 03
             value "-Nao".
      *
       01 tela-mensagem-cad.
          02 line 21 column 11 foreground-color 07 background-color 01
             highlight pic x(59) from mensagem.
      *
       01 tela-erro-cad.
          02 line 21 column 11 beep reverse-video pic x(59) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 21 column 11 foreground-color 01 background-color 01
             pic x(59) from spaces.
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
           move 09 to box-col.
           move 08 to box-lin.
           move 69 to box-col-f.
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
           perform rot-open-cd01.
           perform rot-open-tabl.
           perform sec-selecao.
           perform rot-close-tabl.
           perform rot-close-cd01.
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
       rot-le-tabl.
           move 0 to erro.
           read arqtabl invalid key move 1 to erro.
           if tabl-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-tabl
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
       rot-le-cd01.
           move 0 to erro.
           read arqcd01 invalid key move 1 to erro.
           if cd01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-cd01
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
       rot-erro-categoria.
           move " Categoria nao cadastrada - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
       rot-erro-estado.
            move " Estado nao cadastrado - Tecle <Enter>" to mensagem.
            display tela-erro-cad.
            perform rot-keypress.
      *
       err-codigo-n-c.
            move " Codigo nao cadastrado - Tecle <Enter>" to mensagem.
            display tela-erro-cad.
            perform rot-keypress.
            display tela-limpa-cad.
      *
       rot-open-imp.
           move 0 to erro.
           move param-impress to impress.
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
              move "F" to imp-stat
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
       rot-le-cd02.
           move 0 to erro.
           read arqcd02 invalid key move 1 to erro.
           if cd02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-cd02
           end-if.
      *
       rot-le-cd02-next.
           move 0 to erro.
           read arqcd02 next at end move 1 to erro.
           if cd02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-cd02-next
           end-if.
      *
       rot-posicionar.
           if erro = 0
              move 99999999 to cab-codigo cab-codigo-1 cab-cep cab-cep-1
              move "A/C" to cab-ac cab-ac-1
              move all "X" to cab-razao cab-razao-1 cab-endereco 
                   cab-endereco-1 cab-uf cab-uf-1 cab-cidade 
                   cab-cidade-1 cab-contato cab-contato-1
              write reg-imp from cab-01
              write reg-imp from cab-02
              if sele-contato not = "N"
                 write reg-imp from cab-02-a
              end-if
              write reg-imp from cab-03
              if sele-contato not = "N"
                 write reg-imp from cab-04 before 2 line
              else
                 write reg-imp from cab-04 before 3 line
              end-if
           end-if.
      *
       rot-pesq-cliente.
           perform rot-close-cd01.
           move 12 to rotina-col-cod.
           move 12 to rotina-lin-cod.
           move "3" to rotina-borda-cod.
           move spaces to rotina-fundo-cod.
           move "S" to rotina-sombra-cod.
           call "rotcd01" using param-menu campo-rotina-cod.
           cancel "rotcd01".
           perform rot-open-cd01.
      *
       rot-pesq-tabela.
           perform rot-close-tabl.
           move 12 to rotina-col.
           move 10 to rotina-lin.
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
                display tela-03
             end-if
           end-if.
      *
       rot-contato.
           move cd01-codigo to cd02-codigo.
           move 01 to cd02-contato.
           perform rot-le-cd02.
           
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
           accept resposta at 1968 with auto foreground-color 01
                                             background-color 01.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
      *  Sequencia para dar accept
      *
       acc-codigo-i.
           accept sele-codigo-i at 1134 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-codigo-f.
           accept sele-codigo-f at 1234 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cep-i.
           accept sele-cep-i at 1334 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cep-f.
           accept sele-cep-f at 1434 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-categoria.
           accept sele-categoria at 1534 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept sele-uf at 1634 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-endereco.
           accept sele-endereco at 1734 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-remetente.
           accept sele-remetente at 1834 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-contato.
           accept sele-contato at 1934 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo-i.
           display sele-codigo-i-disp at 1134 with foreground-color 15 
                   background-color 01.
           display sele-dcodigo-i at 1141 with foreground-color 15 
                   background-color 01.
      *
       dsp-codigo-f.
           display sele-codigo-f-disp at 1234 with foreground-color 15 
                   background-color 01.
           display sele-dcodigo-f at 1241 with foreground-color 15 
                   background-color 01.
      *
       dsp-cep-i.
           display sele-cep-i-disp at 1334 with foreground-color 15 
                   background-color 01.
      *
       dsp-cep-f.
           display sele-cep-f-disp at 1434 with foreground-color 15 
                   background-color 01.
      *
       dsp-categoria.
           display sele-categoria at 1534 with foreground-color 15 
                   background-color 01.
           display sele-dcategoria at 1538 with foreground-color 15 
                   background-color 01.
      *
       dsp-uf.
           display sele-uf at 1634 with foreground-color 15 
                   background-color 01.
      *
       dsp-endereco.
           display sele-endereco at 1734 with foreground-color 15 
                   background-color 01.
      *
       dsp-remetente.
           display sele-remetente at 1834 with foreground-color 15 
                   background-color 01.
      *
       dsp-contato.
           display sele-contato-disp at 1934 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza
      *
       lmp-codigo-i.
           display limpa at 1134 with foreground-color 15 
                   background-color 01.
      *
       lmp-codigo-f.
           display limpa at 1234 with foreground-color 15 
                   background-color 01.
      *
       lmp-cep-i.
           display limpa at 1334 with foreground-color 15 
                   background-color 01.
      *
       lmp-cep-f.
           display limpa at 1434 with foreground-color 15 
                   background-color 01.
      *
       lmp-categoria.
           display limpa at 1534 with foreground-color 15 
                   background-color 01.
      *
       lmp-uf.
           display limpa at 1634 with foreground-color 15 
                   background-color 01.
      *
       lmp-endereco.
           display limpa at 1734 with foreground-color 15 
                   background-color 01.
      *
       lmp-remetente.
           display limpa at 1834 with foreground-color 15 
                   background-color 01.
      *
       lmp-contato.
           display limpa at 1934 with foreground-color 15 
                   background-color 01.
      *
       dsp-categoria-todas.
           display "Todas" at 1534 with foreground-color 15 
                   background-color 01.
      *
       dsp-uf-todas.
           display "Todas" at 1634 with foreground-color 15 
                   background-color 01.
      *
       sec-selecao section.
      *
       lab-sele-00-0.
           move 0 to rotina-codigo-cod.
      *
       lab-sele-00.
           display tela-05.
           move rotina-codigo-cod to sele-codigo-i.
           perform lmp-codigo-i.
           perform acc-codigo-i.
           if escape-key = 1
              go to lab-sele-fim
           end-if.
           if escape-key = 3
              perform rot-pesq-cliente
              go to lab-sele-00
           end-if.
           if sele-codigo-i = 0
              move 0 to sele-codigo-i
      *        move 2 to sele-codigo-i
              move "Inicial" to sele-codigo-i-disp
              move spaces to sele-dcodigo-i
              perform dsp-codigo-i
              go to lab-sele-01
           end-if.
           move sele-codigo-i to cd01-codigo sele-codigo-i-disp.
           perform rot-le-cd01.
           if erro not = 0
              perform err-codigo-n-c
              go to lab-sele-00
           end-if.
           move cd01-razao-social-a to sele-dcodigo-i.
           perform dsp-codigo-i.
           move 0 to rotina-codigo-cod.
      *
       lab-sele-01.
           display tela-05.
           move rotina-codigo-cod to sele-codigo-f.
           perform lmp-codigo-f.
           perform acc-codigo-f.
           if escape-key = 1
              perform lmp-codigo-f
              go to lab-sele-00
           end-if.
           if escape-key = 3
              perform rot-pesq-cliente
              go to lab-sele-01
           end-if.
           if sele-codigo-f = 0
              move 99999 to sele-codigo-f
              move "Final" to sele-codigo-f-disp
              move spaces to sele-dcodigo-f
              perform dsp-codigo-f
              display tela-limpa-cad
              go to lab-sele-02
           end-if.
           if sele-codigo-f < sele-codigo-i
              move " Codigo final menor que codigo inicial - Tecle <Ente
      -       "r>" to mensagem
              display tela-erro-cad
              perform rot-keypress
              display tela-limpa-cad
              go to lab-sele-01
           end-if.
           move sele-codigo-f to cd01-codigo sele-codigo-f-disp.
           perform rot-le-cd01.
           if erro not = 0
              perform err-codigo-n-c
              go to lab-sele-01
           end-if.
           move cd01-razao-social-a to sele-dcodigo-f.
           perform dsp-codigo-f.
           display tela-limpa-cad.
      *
       lab-sele-02.
           move 0 to sele-cep-i.
           perform lmp-cep-i.
           perform acc-cep-i.
           if escape-key = 1
              perform lmp-cep-i
              go to lab-sele-01
           end-if.
           if sele-cep-i = 0
              move "Inicial" to sele-cep-i-disp
           else              
              move sele-cep-i to sele-cep-i-aux
              move sele-cep-i-aux to sele-cep-i-disp
           end-if.
           perform dsp-cep-i.
      *
       lab-sele-03.
           move 0 to sele-cep-f.
           perform lmp-cep-f.
           perform acc-cep-f.
           if escape-key = 1
              perform lmp-cep-f
              go to lab-sele-02
           end-if.
           if sele-cep-f = 0
              move 99999999 to sele-cep-f
              move "Final"  to sele-cep-f-disp
           else
              move sele-cep-f to sele-cep-f-aux
              move sele-cep-f-aux to sele-cep-f-disp
           end-if.
           perform dsp-cep-f.
           if sele-cep-f < sele-cep-i
              move " Cep final menor que cep inicial - Tecle <Enter>" 
              to mensagem
              display tela-erro-cad
              perform rot-keypress
              display tela-limpa-cad
              go to lab-sele-03
           end-if.
           move 0 to rotina-codigo.
      *
       lab-sele-04.
           display tela-06.
           move rotina-codigo to sele-categoria.
           move spaces to sele-dcategoria.
           perform lmp-categoria.
           perform acc-categoria.
           if escape-key = 1
              perform lmp-categoria
              display tela-limpa-cad
              go to lab-sele-03
           end-if.
           if escape-key = 3
              move 1 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-sele-04
           end-if.
           if sele-categoria = 0
              perform dsp-categoria-todas
              display tela-limpa-cad
              go to lab-sele-05
           end-if.
           move 01 to wtab01-tipo.
           move sele-categoria to wtab01-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-categoria
              perform rot-keypress
              display tela-limpa-cad
              move spaces to rotina-uf
              go to lab-sele-04
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to sele-dcategoria.
           perform dsp-categoria.
           display tela-limpa-cad.
           move spaces to rotina-uf.
      *
       lab-sele-05.
           display tela-07.
           move rotina-uf to sele-uf.
           perform lmp-uf.
           perform acc-uf.
           if escape-key = 1
              move 0 to rotina-codigo
              perform lmp-uf
              go to lab-sele-04
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-sele-05
           end-if.
           if sele-uf = spaces
              perform dsp-uf-todas
              display tela-limpa-cad
              go to lab-sele-06
           end-if.
           move sele-uf to txt
           perform rot-texto.
           move txt to sele-uf.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move sele-uf to wtab03-sigla.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-estado
              perform rot-keypress
              display tela-limpa-cad
              go to lab-sele-05
           end-if.
           display tela-limpa-cad.
      *
       lab-sele-06.
           display tela-09.
           move spaces to sele-endereco.
           perform lmp-endereco.
           perform acc-endereco.
           if escape-key = 1
              move spaces to rotina-uf
              perform lmp-endereco
              go to lab-sele-05
           end-if.
           move sele-endereco to txt
           perform rot-texto.
           if txt not = "S" and "N"
              go to lab-sele-06
           end-if.
           move txt to sele-endereco.
           perform dsp-endereco.
      *
       lab-sele-07.
           display tela-09.
           move spaces to sele-remetente.
           perform lmp-remetente.
           perform acc-remetente.
           if escape-key = 1
              perform lmp-remetente
              go to lab-sele-06
           end-if.
           move sele-remetente to txt
           perform rot-texto.
           if txt not = "S" and "N"
              go to lab-sele-07
           end-if.
           move txt to sele-remetente.
           perform dsp-remetente.
      *
       lab-sele-08.
           display tela-09.
           move spaces to sele-contato.
           perform lmp-contato.
           perform acc-contato.
           if escape-key = 1
              perform lmp-contato
              go to lab-sele-07
           end-if.
           move sele-contato to txt.
           perform rot-texto.
           if txt not = "S" and "N" and spaces
              go to lab-sele-08
           end-if.
           move txt to sele-contato sele-contato-disp.
           if sele-contato = spaces
              move "Todos" to sele-contato-disp
           end-if.
           perform dsp-contato.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-sele-09.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-08
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-codigo-i thru lmp-contato
              go to lab-sele-00
           else
              if resposta not = "S"
                 go to lab-sele-09
              end-if
           end-if.
           display tela-limpa-cad.
           move spaces to resposta.
           sort arqsort on ascending key reg-sort
                input procedure sec-ord 
                output procedure sec-imp.
           perform lmp-codigo-i thru lmp-contato.
           display tela-limpa-cad.
           go to lab-sele-00-0.
      *
       lab-sele-fim.
           perform lmp-codigo-i thru lmp-contato.
           exit.
      *
       sec-ord section.
      *
       lab-ord-00.
           if sele-categoria = 0
              move sele-codigo-i to cd01-codigo
              start arqcd01 key is not less cd01-chave
           else
              move low-values to cd01-chave-3
              move sele-categoria to cd01-categoria
              start arqcd01 key is not less cd01-chave-3
           end-if.
           display tela-03.
      *
       lab-ord-01.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-ord-fim
           end-if.
           if cd01-chave = high-values
              go to lab-ord-01
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-ord-fim
           end-if.
           if sele-uf not = spaces
              if sele-endereco = "S"
                 if cd01-uf-cbr not = sele-uf
                    go to lab-ord-01
                 else
                    next sentence
              else
                 if cd01-uf not = sele-uf
                    go to lab-ord-01
              end-if
           end-if.
           if sele-categoria not = 0
              if cd01-categoria not = sele-categoria
                 go to lab-ord-fim
              end-if
              if cd01-codigo < sele-codigo-i or 
                 cd01-codigo > sele-codigo-f
                 go to lab-ord-01
              end-if
           else
              if cd01-codigo < sele-codigo-i or 
                 cd01-codigo > sele-codigo-f
                 go to lab-ord-fim
              end-if
           end-if.
           if sele-endereco = "S"
              if cd01-cep-cbr < sele-cep-i or cd01-cep-cbr > sele-cep-f
                 go to lab-ord-01
              end-if
           else
              if cd01-cep < sele-cep-i or cd01-cep > sele-cep-f
                 go to lab-ord-01
              end-if
           end-if.
           if sele-endereco = "S"
              move cd01-cep-cbr to sort-cep
           else
              move cd01-cep to sort-cep
           end-if.
           move cd01-codigo to sort-codigo.
           release reg-sort.
           go to lab-ord-01.
      *
       lab-ord-fim.
           exit.
      *
       sec-imp section.
      *
       lab-imp-00.
           if resposta = "F"
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
           display tela-02.
      *
       lab-imp-01.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-imp-fim
           end-if.
           if resposta = "E"
              display tela-limpa-cad
              go to lab-imp-fim
           else
              if resposta = "P"
                 perform rot-posicionar
                 go to lab-imp-01
              else
                 if resposta not = "I"
                    go to lab-imp-01
                 end-if
              end-if
           end-if.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           display tela-03.
           if sele-remetente = "S"
              move 1 to cd01-codigo
              perform rot-le-cd01
              if erro not = 0
                 go to lab-imp-fim
              end-if
              if sele-contato not = "N"
                 perform rot-contato
                 if erro = 0
                    move "A/C" to cab-ac-1
                    move cd02-nome-a to cab-contato-1
                 else
                    move spaces to cab-ac-1
                    move spaces to cab-contato-1
                 end-if
              end-if
              move cd01-codigo to cab-codigo-1
              move cd01-razao-social-a to cab-razao-1
              move cd01-endereco to cab-endereco-1
              move cd01-cidade to cab-cidade-1
              move cd01-uf to cab-uf-1
              move cd01-cep to cab-cep-1
           end-if.
           move 0 to sub.
      *
       lab-imp-02.
           return arqsort at end 
                  go to lab-imp-fim.
           add 1 to sub.
           move sort-codigo to cd01-codigo.
           perform rot-le-cd01.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if sele-remetente = "S"
              if sele-contato not = "N"
                 perform rot-contato
                 if erro = 0
                    move "A/C" to cab-ac
                    move cd02-nome-a to cab-contato
                 else
                    move spaces to cab-ac
                    move spaces to cab-contato
                 end-if
              end-if
              if sele-endereco = "S"
                 move cd01-codigo to cab-codigo
                 move cd01-razao-social-a to cab-razao
                 move cd01-endereco-cbr to cab-endereco
                 move cd01-cidade-cbr to cab-cidade
                 move cd01-uf-cbr to cab-uf
                 move cd01-cep-cbr to cab-cep
              else
                 move cd01-codigo to cab-codigo
                 move cd01-razao-social-a to cab-razao
                 move cd01-endereco to cab-endereco
                 move cd01-cidade to cab-cidade
                 move cd01-uf to cab-uf
                 move cd01-cep to cab-cep
              end-if
           else
              if sele-contato not = "N"
                 perform rot-contato
                 if erro = 0
                    if sub = 1
                       move "A/C" to cab-ac
                       move cd02-nome-a to cab-contato
                    else
                       move "A/C" to cab-ac-1
                       move cd02-nome-a to cab-contato-1
                    end-if
                 else
                    if sub = 1
                       move spaces to cab-ac
                       move spaces to cab-contato
                    else
                       move spaces to cab-ac-1
                       move spaces to cab-contato-1
                    end-if
                 end-if
              end-if
              if sele-endereco = "S"
                 if sub = 1
                    move cd01-codigo to cab-codigo
                    move cd01-razao-social-a to cab-razao
                    move cd01-endereco-cbr to cab-endereco
                    move cd01-cidade-cbr to cab-cidade
                    move cd01-uf-cbr to cab-uf
                    move cd01-cep-cbr to cab-cep
                    if sele-contato = spaces
                       perform sec-contato
                       if resposta = "F"
                          go to lab-imp-fim
                       end-if
                    end-if
                    go to lab-imp-02
                 else
                    move cd01-codigo to cab-codigo-1
                    move cd01-razao-social-a to cab-razao-1
                    move cd01-endereco-cbr to cab-endereco-1
                    move cd01-cidade-cbr to cab-cidade-1
                    move cd01-uf-cbr to cab-uf-1
                    move cd01-cep-cbr to cab-cep-1
                    move 0 to sub
                 end-if
              else
                 if sub = 1
                    move cd01-codigo to cab-codigo
                    move cd01-razao-social-a to cab-razao
                    move cd01-endereco to cab-endereco
                    move cd01-cidade to cab-cidade
                    move cd01-uf to cab-uf
                    move cd01-cep to cab-cep
                    if sele-contato = spaces
                       perform sec-contato
                       if resposta = "F"
                          go to lab-imp-fim
                       end-if
                    end-if
                    go to lab-imp-02
                 else
                    move cd01-codigo to cab-codigo-1
                    move cd01-razao-social-a to cab-razao-1
                    move cd01-endereco to cab-endereco-1
                    move cd01-cidade to cab-cidade-1
                    move cd01-uf to cab-uf-1
                    move cd01-cep to cab-cep-1
                    move 0 to sub
                 end-if
              end-if
           end-if.
           write reg-imp from cab-01.
           write reg-imp from cab-02.
           if sele-contato not = "N"
              write reg-imp from cab-02-a
           end-if.
           write reg-imp from cab-03.
           if sele-contato not = "N"
              write reg-imp from cab-04 before 2 line
           else
              write reg-imp from cab-04 before 3 line
           end-if.
           if sele-remetente not = "S"
              move spaces to cab-01 cab-02 cab-02-a 
                             cab-03 cab-04 
           end-if.
           if sele-contato = spaces
              perform sec-contato
              if resposta = "F"
                 go to lab-imp-fim
              end-if
           end-if.
           go to lab-imp-02.
      *
       lab-imp-fim.
           if sub = 1 and resposta not = "F" and sele-remetente = "N"
              write reg-imp from cab-01
              write reg-imp from cab-02
              if sele-contato not = "N"
                 write reg-imp from cab-02-a
              end-if
              write reg-imp from cab-03
              if sele-contato not = "N"
                 write reg-imp from cab-04 before 2 line
              else
                 write reg-imp from cab-04 before 3 line
             end-if
           end-if.
           display tela-limpa-cad.
           perform rot-close-imp.
           perform rot-close-cd02.
           exit.
      *
       sec-contato section.
      *
       lab-cont-00.
           move cd01-codigo to cd02-codigo.
           move 02 to cd02-contato.
           start arqcd02 key is not less cd02-chave invalid key
                 move 1 to erro
                 go to lab-cont-fim
           end-start.
      *
       lab-cont-01.
           perform rot-le-cd02-next.
           if erro not = 0
              go to lab-cont-fim
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-cont-fim
           end-if.
           if cd02-codigo not = cd01-codigo
              go to lab-cont-fim
           end-if.
           add 1 to sub.
           if sele-remetente = "S"
              move "A/C" to cab-ac
              move cd02-nome-a to cab-contato
              if sele-endereco = "S"
                 move cd01-codigo to cab-codigo
                 move cd01-razao-social-a to cab-razao
                 move cd01-endereco-cbr to cab-endereco
                 move cd01-cidade-cbr to cab-cidade
                 move cd01-uf-cbr to cab-uf
                 move cd01-cep-cbr to cab-cep
              else
                 move cd01-codigo to cab-codigo
                 move cd01-razao-social-a to cab-razao
                 move cd01-endereco to cab-endereco
                 move cd01-cidade to cab-cidade
                 move cd01-uf to cab-uf
                 move cd01-cep to cab-cep
              end-if
           else
              if sub = 1
                 move "A/C" to cab-ac
                 move cd02-nome-a to cab-contato
              else
                 move "A/C" to cab-ac-1
                 move cd02-nome-a to cab-contato-1
              end-if
              if sele-endereco = "S"
                 if sub = 1
                    move cd01-codigo to cab-codigo
                    move cd01-razao-social-a to cab-razao
                    move cd01-endereco-cbr to cab-endereco
                    move cd01-cidade-cbr to cab-cidade
                    move cd01-uf-cbr to cab-uf
                    move cd01-cep-cbr to cab-cep
                    go to lab-cont-01
                 else
                    move cd01-codigo to cab-codigo-1
                    move cd01-razao-social-a to cab-razao-1
                    move cd01-endereco-cbr to cab-endereco-1
                    move cd01-cidade-cbr to cab-cidade-1
                    move cd01-uf-cbr to cab-uf-1
                    move cd01-cep-cbr to cab-cep-1
                    move 0 to sub
                 end-if
              else
                 if sub = 1
                    move cd01-codigo to cab-codigo
                    move cd01-razao-social-a to cab-razao
                    move cd01-endereco to cab-endereco
                    move cd01-cidade to cab-cidade
                    move cd01-uf to cab-uf
                    move cd01-cep to cab-cep
                    go to lab-cont-01
                 else
                    move cd01-codigo to cab-codigo-1
                    move cd01-razao-social-a to cab-razao-1
                    move cd01-endereco to cab-endereco-1
                    move cd01-cidade to cab-cidade-1
                    move cd01-uf to cab-uf-1
                    move cd01-cep to cab-cep-1
                    move 0 to sub
                 end-if
              end-if
           end-if.
           write reg-imp from cab-01.
           write reg-imp from cab-02.
           if sele-contato not = "N"
              write reg-imp from cab-02-a
           end-if.
           write reg-imp from cab-03.
           if sele-contato not = "N"
              write reg-imp from cab-04 before 2 line
           else
              write reg-imp from cab-04 before 3 line
           end-if.
           if sele-remetente not = "S"
              move spaces to cab-01 cab-02 cab-02-a 
                             cab-03 cab-04 
           end-if.
           go to lab-cont-01.
      *
       lab-cont-fim.
           exit.
