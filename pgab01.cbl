      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGAB01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao do cadastro de Associados :                     *
      *                                                             *
      *  Data da ultima alteracao:    21/12/93     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgab01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqab01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab01-chave
                  alternate record key is ab01-chave-1 with duplicates
                  alternate record key is ab01-chave-2 with duplicates
                  file status is ab01-status.
      *
           select arqab02 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab02-chave
                  alternate record key is ab02-chave-1 with duplicates
                  file status is ab02-status.
      *
           select arqab04 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab04-chave
                  alternate record key is ab04-chave-1 with duplicates
                  alternate record key is ab04-chave-2 with duplicates
                  file status is ab04-status.
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
       copy fdab01.lib.
      *    
       copy fdab02.lib.
      *    
       copy fdab04.lib.
      *
       copy fdtabl.lib.
      *
       working-storage section.
      *
       01 ab01-status                  pic x(02) value "00".
       01 ab01-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab01.
          02 ab01-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab01-nome                 pic x(08) value "ARQAB01A".
          02 filler                    pic x(01) value ".".
          02 ab01-ext                  pic x(03) value "DAT".
      *
       01 ab02-status                  pic x(02) value "00".
       01 ab02-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab02.
          02 ab02-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab02-nome                 pic x(08) value "ARQAB02A".
          02 filler                    pic x(01) value ".".
          02 ab02-ext                  pic x(03) value "DAT".
      *
       01 ab04-status                  pic x(02) value "00".
       01 ab04-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab04.
          02 ab04-dir                  pic x(03) value "AB2".
          02 filler                    pic x(01) value "\".
          02 ab04-nome-arq             pic x(08) value "ARQAB04A".
          02 filler                    pic x(01) value ".".
          02 ab04-ext                  pic x(03) value "DAT".
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
          02 cb-programa               pic x(08) value "PGAB01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(40) value spaces.
       01 limpa-03                     pic x(03) value spaces.
       01 limpa-05                     pic x(05) value spaces.
       01 limpa-08                     pic x(08) value spaces.
       01 limpa-12                     pic x(12) value spaces.
       01 limpa-15                     pic x(15) value spaces.
       01 limpa-20                     pic x(20) value spaces.
       01 limpa-30                     pic x(30) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 codigo                    pic 9(05) value 0.
          02 condicao                  pic x(01) value spaces.
          02 nome-fantasia             pic x(40) value spaces.
          02 nome-fantasia-a           pic x(40) value spaces.
          02 razao-social              pic x(40) value spaces.
          02 razao-social-a            pic x(40) value spaces.
          02 endereco                  pic x(40) value spaces.
          02 cidade                    pic x(20) value spaces.
          02 uf                        pic x(02) value spaces.
          02 cep                       pic 9(08) value 0.
          02 cep-disp                  pic 9(05)b9(03) value 0.
          02 ddd                       pic 9(04) value 0.
          02 telefone                  pic x(08) occurs 2.
          02 ramal                     pic 9(03) occurs 2 
             blank when zero.
          02 telex                     pic x(08) value spaces.
          02 fax                       pic x(08) value spaces.
          02 capital                   pic x(01) value spaces.
          02 sindicato                 pic 9(05) value 0 
             blank when zero.
          02 snea                      pic 9(05) value 0
             blank when zero.
          02 embratur                  pic x(12) value spaces.
          02 iata                      pic x(12) value spaces.
          02 asta                      pic x(12) value spaces.
          02 cotal                     pic x(12) value spaces.
          02 cgc                       pic 9(14) value 0.
          02 cgc-disp                  pic 99.999.999/9999b99
             blank when zero.
          02 situacao                  pic 9(01) value 0.
          02 categoria                 pic 9(03) value 0.
          02 dcategoria                pic x(25) value spaces.
          02 data-alt                  pic 9(06) value 0.
          02 data-alt-disp             pic x(08) value spaces.
          02 data-inc                  pic 9(06) value 0.
          02 data-inc-disp             pic x(08) value spaces.
          02 data-exc                  pic 9(06) value 0.
          02 data-exc-disp             pic x(08) value spaces.
      *
       01 campos-t.
          02 t-titular                 pic 9(02) value 0.
          02 t-titular-aux             pic 9(02) value 0.
          02 t-nome                    pic x(40) value spaces.
          02 t-nome-a                  pic x(40) value spaces.
          02 t-endereco                pic x(40) value spaces.
          02 t-cidade                  pic x(20) value spaces.
          02 t-uf                      pic x(02) value spaces.
          02 t-cep                     pic 9(08) value 0.
          02 t-cep-disp                pic 9(05)b9(03) value 0.
          02 t-ddd                     pic 9(04) value 0.
          02 t-telefone                pic x(08) value spaces.
          02 t-data                    pic 9(06) value 0.
          02 t-data-disp               pic x(08) value spaces.
          02 t-nascionalidade          pic x(15) value spaces.
          02 t-rg                      pic z(09) value 0.
          02 t-rg-disp                 pic 999.999.999 value 0
             blank when zero.
          02 t-emissor                 pic x(10) value spaces.
          02 t-cpf                     pic z(11) value 0.
          02 t-cpf-disp                pic 999.999.999b99 value spaces
             blank when zero.
          02 t-cargo                   pic x(10) value spaces.
          02 t-controle                pic x(53) value spaces.
          02 t-flag                    pic 9(02) value 0.
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
       01 buffer1.
          02 filler                    pic 9(04) occurs 2180.
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
          02 line 07 column 05 foreground-color 06 background-color 01
             highlight value "Codigo.......:".
          02 line 07 column 33 foreground-color 06 background-color 01
             highlight value "Condicao.:".
          02 line 08 column 05 foreground-color 06 background-color 01
             highlight value "Razao Social.:".
          02 line 09 column 05 foreground-color 06 background-color 01
             highlight value "Nome Fantasia:".
          02 line 11 column 05 foreground-color 06 background-color 01
             highlight value "Endereco.....:".
          02 line 12 column 05 foreground-color 06 background-color 01
             highlight value "UF...........:".
          02 line 12 column 24 foreground-color 06 background-color 01
             highlight value "Cidade.:".
          02 line 12 column 56 foreground-color 06 background-color 01
             highlight value "C.E.P.:".
          02 line 13 column 05 foreground-color 06 background-color 01
             highlight value "Telefone.....:".
          02 line 13 column 53 foreground-color 06 background-color 01
             highlight value "Telex...:".
          02 line 14 column 05 foreground-color 06 background-color 01
             highlight value "Fax..........:".
          02 line 14 column 33 foreground-color 06 background-color 01
             highlight value "Ramal AB.:".
          02 line 14 column 53 foreground-color 06 background-color 01
             highlight value "D.D.D...:".
          02 line 15 column 05 foreground-color 06 background-color 01
             highlight value "Sindicato....:".
          02 line 15 column 33 foreground-color 06 background-color 01
             highlight value "SNEA.....:".
          02 line 15 column 53 foreground-color 06 background-color 01
             highlight value "Embratur:".
          02 line 16 column 05 foreground-color 06 background-color 01
             highlight value "IATA.........:".
          02 line 16 column 33 foreground-color 06 background-color 01
             highlight value "COTAL....:".
          02 line 17 column 05 foreground-color 06 background-color 01
             highlight value "C.G.C........:".
          02 line 18 column 05 foreground-color 06 background-color 01
             highlight value "Situacao.....:".
          02 line 18 column 33 foreground-color 06 background-color 01
             highlight value "Categoria:".
          02 line 19 column 05 foreground-color 06 background-color 01
             highlight value "Alt.Contrato.:".
          02 line 19 column 33 foreground-color 06 background-color 01
             highlight value "Adesao...:".
          02 line 19 column 53 foreground-color 06 background-color 01
             highlight value "Cancel..:".
      *
       01 tela-02.
          02 line 21 column 04 foreground-color 02 background-color 03
             highlight pic x(72) from spaces.
          02 line 21 column 05 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 21 column 07 foreground-color 05 background-color 03
             value "-Help".
          02 line 21 column 15 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 21 column 17 foreground-color 05 background-color 03
             value "-Consultas".
      *
       01 tela-03.
          02 line 21 column 04 foreground-color 02 background-color 03
             highlight pic x(72) from spaces.
          02 line 21 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 21 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 21 column 16 foreground-color 02 background-color 03
             highlight value "C".
          02 line 21 column 17 foreground-color 05 background-color 03
             value "odigo".
          02 line 21 column 27 foreground-color 02 background-color 03
             highlight value "R".
          02 line 21 column 28 foreground-color 05 background-color 03
             value "azao Social".
          02 line 21 column 43 foreground-color 02 background-color 03
             highlight value "N".
          02 line 21 column 44 foreground-color 05 background-color 03
             value "ome Fantasia".
      *
       01 tela-04.
          02 line 21 column 04 foreground-color 02 background-color 03
             highlight pic x(72) from spaces.
          02 line 21 column 05 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 07 foreground-color 05 background-color 03
             value "-Alt".
          02 line 21 column 13 foreground-color 02 background-color 03 
             highlight value "F3".
          02 line 21 column 15 foreground-color 05 background-color 03
             value "-Exc".
          02 line 21 column 21 foreground-color 02 background-color 03 
             highlight value "F4".
          02 line 21 column 23 foreground-color 05 background-color 03
             value "-Tit".
          02 line 21 column 29 foreground-color 02 background-color 03
             highlight value "Home".
          02 line 21 column 33 foreground-color 05 background-color 03
             value "-Inic".
          02 line 21 column 41 foreground-color 02 background-color 03
             highlight value "End".
          02 line 21 column 44 foreground-color 05 background-color 03
             value "-Fim".
          02 line 21 column 51 foreground-color 02 background-color 03
             highlight value "PgDown".
          02 line 21 column 57 foreground-color 05 background-color 03
             value "-Prox".
          02 line 21 column 64 foreground-color 02 background-color 03
             highlight value "PgUp".
          02 line 21 column 68 foreground-color 05 background-color 03
             value "-Ant".
      *
       01 tela-05.
          02 line 21 column 04 foreground-color 02 background-color 03
             highlight pic x(72) from spaces.
          02 line 21 column 05 foreground-color 02 background-color 03 
             highlight value "A".
          02 line 21 column 06 foreground-color 05 background-color 03
             value " - Associados".
         02 line 21 column 23 foreground-color 02 background-color 03 
             highlight value "F".
          02 line 21 column 24 foreground-color 05 background-color 03
             value " - Afiliados".
      *
       01 tela-06.
          02 line 21 column 04 foreground-color 02 background-color 03
             highlight pic x(72) from spaces.
          02 line 21 column 04 foreground-color 02 background-color 03
             highlight value "Registro gravado - Tecle <Enter>".
          02 line 21 column 38 foreground-color 02 background-color 03 
             highlight value "F4".
          02 line 21 column 40 foreground-color 05 background-color 03
             value "-Titular".
      *
       01 tela-07.
          02 line 06 column 68 foreground-color 06 background-color 01
             highlight value "Inclusao".
      *
       01 tela-08.
          02 line 06 column 68 foreground-color 06 background-color 01
             highlight value "Consulta".
      *
       01 tela-09.
          02 line 21 column 04 foreground-color 02 background-color 03
             highlight pic x(72) from spaces.
          02 line 21 column 05 foreground-color 02 background-color 03 
             highlight value "1".
          02 line 21 column 06 foreground-color 05 background-color 03
             value "-Ativo".
          02 line 21 column 15 foreground-color 02 background-color 03 
             highlight value "2".
          02 line 21 column 16 foreground-color 05 background-color 03
             value "-CLD Solicitacao".
          02 line 21 column 35 foreground-color 02 background-color 03 
             highlight value "3".
          02 line 21 column 36 foreground-color 05 background-color 03
             value "-CLD Debito".
          02 line 21 column 49 foreground-color 02 background-color 03 
             highlight value "4".
          02 line 21 column 50 foreground-color 05 background-color 03
             value "-Falencia".
          02 line 21 column 62 foreground-color 02 background-color 03 
             highlight value "5".
          02 line 21 column 63 foreground-color 05 background-color 03
             value "-Suspensao".
      *
       01 tela-10.
          02 line 21 column 04 foreground-color 02 background-color 03
             highlight pic x(72) from spaces.
          02 line 21 column 05 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 07 foreground-color 05 background-color 03
             value " - Estados".
      *
       01 tela-11.
          02 line 21 column 04 foreground-color 02 background-color 03
             highlight pic x(72) from spaces.
          02 line 21 column 05 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 21 column 07 foreground-color 05 background-color 03
             value " - Categorias".
      *
       01 tela-12.
          02 line 09 column 09 foreground-color 06 background-color 01
             highlight value "Titular..:".
          02 line 10 column 09 foreground-color 06 background-color 01
             highlight value "Endereco.:".
          02 line 11 column 09 foreground-color 06 background-color 01
             highlight value "U.F......:".
          02 line 11 column 24 foreground-color 06 background-color 01
             highlight value "Cidade..:".
          02 line 11 column 55 foreground-color 06 background-color 01
             highlight value "D.D.D.:".
          02 line 12 column 09 foreground-color 06 background-color 01
             highlight value "C.E.P....:".
          02 line 12 column 36 foreground-color 06 background-color 01
             highlight value "Telefone......:".
          02 line 13 column 09 foreground-color 06 background-color 01
             highlight value "Dt. Nasc.:".
          02 line 13 column 36 foreground-color 06 background-color 01
             highlight value "Nascinalidade.:".
          02 line 14 column 09 foreground-color 06 background-color 01
             highlight value "R.G......:".
          02 line 14 column 36 foreground-color 06 background-color 01
             highlight value "Emissor.......:".
          02 line 15 column 09 foreground-color 06 background-color 01
             highlight value "C.P.F....:".
          02 line 15 column 36 foreground-color 06 background-color 01
             highlight value "Cargo.........:".
      *
       01 tela-13.
          02 line 08 column 63 foreground-color 06 background-color 01
             highlight value "Inclusao".
      *
       01 tela-14.
          02 line 08 column 63 foreground-color 06 background-color 01
             highlight value "Consulta".
      *
       01 tela-15.
          02 line 17 column 08 foreground-color 02 background-color 03
             highlight pic x(63) from spaces.
          02 line 17 column 09 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 17 column 11 foreground-color 05 background-color 03
             value "-Help".
          02 line 17 column 19 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 17 column 21 foreground-color 05 background-color 03
             value "-Consultas".
      *
       01 tela-16.
          02 line 17 column 08 foreground-color 02 background-color 03
             highlight pic x(63) from spaces.
          02 line 17 column 09 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 17 column 11 foreground-color 05 background-color 03
             value " - Estados".
      *
       01 tela-17.
          02 line 17 column 08 foreground-color 02 background-color 03
             highlight pic x(63) from spaces.
          02 line 17 column 08 foreground-color 02 background-color 03
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-18.
          02 line 17 column 08 foreground-color 02 background-color 03
             highlight pic x(63) from spaces.
          02 line 17 column 10 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 17 column 12 foreground-color 05 background-color 03
             value "-Help".
          02 line 17 column 20 foreground-color 02 background-color 03
             highlight value "T".
          02 line 17 column 21 foreground-color 05 background-color 03
             value "itular".
          02 line 17 column 31 foreground-color 02 background-color 03
             highlight value "N".
          02 line 17 column 32 foreground-color 05 background-color 03
             value "ome Titular".
      *
       01 tela-19.
          02 line 17 column 08 foreground-color 02 background-color 03
             highlight pic x(63) from spaces.
          02 line 17 column 09 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 17 column 11 foreground-color 05 background-color 03
             value "-Alt".
          02 line 17 column 17 foreground-color 02 background-color 03 
             highlight value "F3".
          02 line 17 column 19 foreground-color 05 background-color 03
             value "-Exc".
          02 line 17 column 26 foreground-color 02 background-color 03
             highlight value "Home".
          02 line 17 column 30 foreground-color 05 background-color 03
             value "-Inic".
          02 line 17 column 38 foreground-color 02 background-color 03
             highlight value "End".
          02 line 17 column 41 foreground-color 05 background-color 03
             value "-Fim".
          02 line 17 column 47 foreground-color 02 background-color 03
             highlight value "PgDown".
          02 line 17 column 53 foreground-color 05 background-color 03
             value "-Prox".
          02 line 17 column 61 foreground-color 02 background-color 03
             highlight value "PgUp".
          02 line 17 column 65 foreground-color 05 background-color 03
             value "-Ant".
      *
       01 tela-mensagem-cad.
          02 line 21 column 04 foreground-color 07 background-color 01
             highlight pic x(72) from mensagem.
      *
       01 tela-erro-cad.
          02 line 21 column 04 beep reverse-video pic x(72) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 21 column 04 foreground-color 01 background-color 01
             pic x(72) from spaces.
      *
       01 tela-mensagem-cad-t.
          02 line 17 column 08 foreground-color 07 background-color 01
             highlight pic x(63) from mensagem.
      *
       01 tela-erro-cad-t.
          02 line 17 column 08 beep reverse-video pic x(63) from 
             mensagem.
      *
       01 tela-limpa-cad-t.
          02 line 17 column 08 foreground-color 01 background-color 01
             pic x(63) from spaces.
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
           move 75 to box-col-f.
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
       rot-move-ab01.
           move codigo to ab01-codigo.
           move condicao to ab01-condicao.
           move razao-social to ab01-razao-social.
           move razao-social-a to ab01-razao-social-a.
           move nome-fantasia to ab01-nome-fantasia.
           move nome-fantasia-a to ab01-nome-fantasia-a.
           move endereco to ab01-endereco.
           move uf to ab01-uf.
           move cidade to ab01-cidade.
           move cep to ab01-cep.
           move telefone (01) to ab01-telefone (01).
           move telefone (02) to ab01-telefone (02).
           move telex to ab01-telex.
           move fax to ab01-fax.
           move ramal (01) to ab01-ramal (01).
           move ramal (02) to ab01-ramal (02).
           move ddd to ab01-ddd.
           move sindicato to ab01-sindicato.
           move snea to ab01-snea.
           move embratur to ab01-embratur.
           move iata to ab01-iata.
           move cotal to ab01-cotal.
           move situacao to ab01-situacao.
           move categoria to ab01-categoria.
           move data-alt to ab01-data-alt.
           move data-inc to ab01-data-inc.
           move data-exc to ab01-data-exc.
           move cgc to ab01-cgc.
           move param-usr to ab01-usuario.
           move param-data to ab01-data.
           
      *
       rot-move-campos.
           move ab01-codigo to codigo.
           move ab01-condicao to condicao.
           move ab01-razao-social-a to razao-social.
           move ab01-nome-fantasia-a to nome-fantasia.
           move ab01-endereco to endereco.
           move ab01-uf to uf.
           move ab01-cidade to cidade.
           move ab01-cep to cep cep-disp.
           move ab01-telefone (01) to telefone (01).
           move ab01-telefone (02) to telefone (02).
           move ab01-telex to telex.
           move ab01-fax to fax.
           move ab01-ramal (01) to ramal (01).
           move ab01-ramal (02) to ramal (02).
           move ab01-ddd to ddd.
           move ab01-sindicato to sindicato.
           move ab01-snea to snea.
           move ab01-embratur to embratur.
           move ab01-iata to iata.
           move ab01-cotal to cotal.
           move ab01-situacao to situacao.
           move ab01-categoria to categoria.
           move ab01-data-alt to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to data-alt-disp.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move data-aux to data-alt.
           move ab01-data-inc to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to data-inc-disp.
           move dia-euro to dia-aux.
           move mes-euro to mes-aux.
           move ano-euro to ano-aux.
           move data-aux to data-inc.
           if ab01-data-exc not = 0
              move ab01-data-exc to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to data-exc-disp
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-exc
           else
              move 0 to data-exc
              move spaces to data-exc-disp
           end-if.
           move ab01-cgc to cgc cgc-disp.
           move ab01-usuario to cab-usuario.
           move ab01-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
      *
       rot-move-ab04.
           move ab01-codigo to ab04-codigo.
           move ab01-condicao to ab04-condicao.
           move t-titular to ab04-titular.
           move t-nome to ab04-nome.
           move t-nome-a to ab04-nome-a.
           move t-endereco to ab04-endereco.
           move t-cidade to ab04-cidade.
           move t-uf to ab04-uf.
           move t-cep to ab04-cep.
           move t-ddd to ab04-ddd.
           move t-telefone to ab04-telefone.
           move t-data to ab04-data-nasc.
           move t-nascionalidade to ab04-nascionalidade.
           move t-rg to ab04-rg.
           move t-emissor to ab04-emissor.
           move t-cpf to ab04-cpf
           move t-cargo to ab04-cargo.
           move param-usr to ab04-usuario.
           move param-data to ab04-data.
      *
       rot-move-campos-tit.
           move ab04-titular to t-titular.
           move ab04-nome-a to t-nome.
           move ab04-endereco to t-endereco.
           move ab04-cidade to t-cidade.
           move ab04-uf to t-uf.
           move ab04-cep to t-cep t-cep-disp.
           move ab04-ddd to t-ddd.
           move ab04-telefone to t-telefone.
           if ab04-data-nasc not = 0
              move ab04-data-nasc to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to t-data
              move data-disp to t-data-disp
           else
              move spaces to t-data-disp
              move 0 to t-data
           end-if.
           move ab04-nascionalidade to t-nascionalidade.
           move ab04-rg to t-rg t-rg-disp.
           move ab04-emissor to t-emissor.
           move ab04-cpf to t-cpf t-cpf-disp
           move ab04-cargo to t-cargo
           move ab04-usuario to cab-usuario.
           move ab04-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
      *
       rot-le-ab01.
           move 0 to erro.
           read arqab01 invalid key move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab01
           end-if.
      *
       rot-le-ab01-1.
           move 0 to erro.
           read arqab01 key ab01-chave-1 invalid key move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab01-1
           end-if.
      *
       rot-le-ab01-2.
           move 0 to erro.
           read arqab01 key ab01-chave-2 invalid key move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab01-2
           end-if.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqab01 key is equal ab01-chave invalid key
                 move 1 to erro
                 perform err-leitura-ab01
           end-start.
      *
       rot-le-ab01-lock.
           move 0 to erro.
           read arqab01 next. 
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ab01-lock
           end-if.
           read arqab01 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqab01 previous at end move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqab01 next at end move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ab01.
           move 0 to erro.
           if ab01-stat = "F"
              open i-o arqab01
              if ab01-status not = "00"
                 move 
                 " Erro de abertura no ARQAB01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab01-stat
               end-if
           end-if.
      *
       rot-close-ab01.
           if ab01-stat = "A"
              close arqab01
              move "F" to ab01-stat
           end-if.
      *
       err-leitura-ab01.
           move " Erro de leitura - ARQAB01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-open-ab02.
           move 0 to erro.
           if ab02-stat = "F"
              open i-o arqab02
              if ab02-status not = "00"
                 move 
                 " Erro de abertura no ARQAB02A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab02-stat
               end-if
           end-if.
      *
       rot-close-ab02.
           if ab02-stat = "A"
              close arqab02
              move "F" to ab02-stat
           end-if.
      *
       rot-erro-leitura-ab02.
           move " Erro de leitura - ARQAB02A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-ponteiro-ab02.
           move 0 to erro.
           start arqab02 key is equal ab02-chave invalid key
                 move 1 to erro
           end-start.
      *
       rot-le-ab02-lock.
           move 0 to erro.
           read arqab02 next. 
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ab02-lock
           end-if.
           read arqab02 with kept lock.
      *
       rot-rewrite-ab02.
           move codigo to ab02-codigo.
           perform rot-ponteiro-ab02.
           if erro = 0
              perform rot-le-ab02-lock
              move razao-social to ab02-razao-social
              move razao-social-a to ab02-razao-social-a
              rewrite reg-ab02 invalid key 
                      move 1 to erro
                      move " Erro de regravacao - ARQAB02A.DAT - Tecle <
      -               "Enter>" to mensagem
                      display tela-erro
                      perform rot-keypress
                      display tela-limpa
              end-rewrite
           end-if.
      *
       rot-open-ab04.
           move 0 to erro.
           if ab04-stat = "F"
              open i-o arqab04
              if ab04-status not = "00"
                 move 
                 " Erro de abertura no ARQAB04A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to ab04-stat
               end-if
           end-if.
      *
       rot-close-ab04.
           if ab04-stat = "A"
              close arqab04
              move "F" to ab04-stat
           end-if.
      *
       err-leitura-ab04.
           move " Erro de leitura - ARQAB04A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-ab04-next.
           move 0 to erro.
           read arqab04 next at end move 1 to erro.
           if ab04-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab04-next
           end-if.
      *
       rot-le-ab04-prev.
           move 0 to erro.
           read arqab04 previous at end move 1 to erro.
           if ab04-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab04-prev
           end-if.
      *
       rot-le-ab04.
           move 0 to erro.
           read arqab04 invalid key move 1 to erro.
           if ab04-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab04
           end-if.
      *
       rot-ponteiro-ab04.
           move 0 to erro.
           start arqab04 key is equal ab04-chave invalid key
                 move 1 to erro
                 perform err-leitura-ab04
           end-start.
      *
       rot-le-ab04-lock.
           move 0 to erro.
           read arqab04 next. 
           if ab04-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ab04-lock
           end-if.
           read arqab04 with kept lock.
      *
       rot-nome-cad.
           move " Nome ja cadastrado - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-data-i.
           move " Data invalida - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-data-i-t.
           move " Data invalida - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad-t.
           perform rot-keypress.
           display tela-limpa-cad-t.
      *
       rot-data-m.
           move " Data final menor que data inicial - Tecle <Enter>" 
           to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-inic-arquivo.
           perform lmp-codigo thru lmp-data-exc.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-inic-arquivo-t.
           perform lmp-t-titular thru lmp-t-cargo.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-codigo thru lmp-data-exc.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo-t.
           perform lmp-t-titular thru lmp-t-cargo.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-display.
           perform rot-move-campos.
           move 01 to wtab01-tipo.
           move categoria to wtab01-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              move "Categoria nao cadastrada" to dcategoria
           else
              move reg-tabl to reg-wtab01
              move wtab01-descricao to dcategoria
           end-if.
           perform dsp-codigo thru dsp-data-exc.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       rot-display-tit.
           perform rot-move-campos-tit.
           perform dsp-t-titular thru dsp-t-cargo.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
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
       err-categoria.
           move " Categoria nao cadastrada - Tecle <Enter>" to mensagem.
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
       err-estado-t.
            move " Estado nao cadastrado - Tecle <Enter>" to mensagem.
            display tela-erro-cad-t.
            perform rot-keypress.
            display tela-16.
      *
       err-tit-c.
           move " Titular ja cadastrado - Tecle <Enter>" 
           to mensagem.
           display tela-erro-cad-t.
           perform rot-keypress.
           display tela-limpa-cad-t.
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
       sec-acha-titular section.
      *
       rot-acha-tit-00.
           move 0 to erro.
           move t-nome to ab04-chave-1.
           start arqab04 key is equal ab04-chave-1 
                 invalid key move 1 to erro
           end-start.
           if erro not = 0
              move 1 to erro
              go to rot-acha-tit-fim
           end-if.
      *
       rot-acha-tit-01.
           perform rot-le-ab04-next.
           if erro not = 0 or ab04-chave = high-values
              move 1 to erro
              go to rot-acha-tit-fim
           end-if.
           if ab04-nome not = t-nome
               move 1 to erro
              go to rot-acha-tit-fim
           end-if.
           if ab04-codigo = ab01-codigo and 
              ab04-condicao = ab01-condicao and
              ab04-titular not = t-titular-aux
              move 0 to erro
              go to rot-acha-tit-fim
           end-if.
           go to rot-acha-tit-01.
      *
       rot-acha-tit-fim.
           exit.
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
           accept resposta at 1974 with auto foreground-color 01
                                             background-color 01.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
       accept-resposta-cad-t.
           move spaces to resposta.
           accept resposta at 1668 with auto foreground-color 01
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
       display-erro-usr-t.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad-t.
           perform rot-keypress.
      *
      *  Sequencia para dar Accept (Associados)
      *
       acc-codigo.
           accept codigo at 0720 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-condicao.
           accept condicao at 0744 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-razao.
           accept razao-social at 0820 with auto update prompt 
                  highlight
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-nome.
           accept nome-fantasia at 0920 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-endereco.
           accept endereco at 1120 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept uf at 1220 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cidade.
           accept cidade at 1233 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cep.
           accept cep at 1264 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-tel-01.
           accept telefone (01) at 1320 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-tel-02.
           accept telefone (02) at 1333 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-telex.
           accept telex at 1363 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-fax.
           accept fax at 1420 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-ramal-01.
           accept ramal (01) at 1444 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-ramal-02.
           accept ramal (02) at 1448 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-ddd.
           accept ddd at 1463 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-sindicato.
           accept sindicato at 1520 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-snea.
           accept snea at 1544 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-embratur.
           accept embratur at 1563 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-iata.
           accept iata at 1620 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cotal.
           accept cotal at 1644 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cgc.
           accept cgc at 1720 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-situacao.
           accept situacao at 1820 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-categoria.
           accept categoria at 1844 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-alt.
           accept data-alt at 1920 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-inc.
           accept data-inc at 1944 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-exc.
           accept data-exc at 1963 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar Accept (Titulares)
      *
       acc-t-titular.
           accept t-titular at 0920 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-nome.
           accept t-nome at 0924 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-endereco.
           accept t-endereco at 1020 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-uf.
           accept t-uf at 1120 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-cidade.
           accept t-cidade at 1134 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-ddd.
           accept t-ddd at 1163 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-cep.
           accept t-cep at 1220 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-telefone.
           accept t-telefone at 1252 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-data.
           accept t-data at 1320 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-nascionalidade.
           accept t-nascionalidade at 1352 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-rg.
           accept t-rg at 1420 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-emissor.
           accept t-emissor at 1452 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-cpf.
           accept t-cpf at 1520 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-t-cargo.
           accept t-cargo at 1552 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar Display (Associados)
      *
       dsp-codigo.
           display codigo at 0720 with foreground-color 15 
                   background-color 01.
      *
       dsp-condicao.
           display condicao at 0744 with foreground-color 15 
                   background-color 01.
      *
       dsp-razao.
           display razao-social at 0820 with foreground-color 15 
                   background-color 01.
      *
       dsp-nome.
           display nome-fantasia at 0920 with foreground-color 15 
                   background-color 01.
      *
       dsp-endereco.
           display endereco at 1120 with foreground-color 15 
                   background-color 01.
      *
       dsp-uf.
           display uf at 1220 with foreground-color 15 
                   background-color 01.
      *
       dsp-cidade.
           display cidade at 1233 with foreground-color 15 
                   background-color 01.
      *
       dsp-cep.
           display cep-disp at 1264 with foreground-color 15 
                   background-color 01.
      *
       dsp-tel-01.
           display telefone (01) at 1320 with foreground-color 15 
                   background-color 01.
      *
       dsp-tel-02.
           display telefone (02) at 1333 with foreground-color 15 
                   background-color 01.
      *
       dsp-telex.
           display telex at 1363 with foreground-color 15 
                   background-color 01.
      *
       dsp-fax.
           display fax at 1420 with foreground-color 15 
                   background-color 01.
      *
       dsp-ramal-01.
           if ramal (01) not = 0
              display ramal (01) at 1444 with foreground-color 15 
                      background-color 01
           else
              display limpa-03 at 1444 with foreground-color 15 
                      background-color 01
           end-if.
      *
       dsp-ramal-02.
           display ramal (02) at 1448 with foreground-color 15 
                   background-color 01.
      *
       dsp-ddd.
           display ddd at 1463 with foreground-color 15 
                   background-color 01.
      *
       dsp-sindicato.
           display sindicato at 1520 with foreground-color 15 
                   background-color 01.
      *
       dsp-snea.
           display snea at 1544 with foreground-color 15 
                   background-color 01.
      *
       dsp-embratur.
           display embratur at 1563 with foreground-color 15 
                   background-color 01.
      *
       dsp-iata.
           display iata at 1620 with foreground-color 15 
                   background-color 01.
      *
       dsp-cotal.
           display cotal at 1644 with foreground-color 15 
                   background-color 01.
      *
       dsp-cgc.
           display cgc-disp at 1720 with foreground-color 15 
                   background-color 01.
      *
       dsp-situacao.
           display situacao at 1820 with foreground-color 15 
                   background-color 01.
      *
       dsp-categoria.
           display categoria at 1844 with foreground-color 15 
                   background-color 01.
           display dcategoria at 1849 with foreground-color 15 
                   background-color 01.
      *
       dsp-data-alt.
           display data-alt-disp at 1920 with foreground-color 15 
                   background-color 01.
      *
       dsp-data-inc.
           display data-inc-disp at 1944 with foreground-color 15 
                   background-color 01.
      *
       dsp-data-exc.
           display data-exc-disp at 1963 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para dar Display (Titulares)
      *
       dsp-t-titular.
           display t-titular at 0920 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-nome.
           display t-nome at 0924 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-endereco.
           display t-endereco at 1020 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-uf.
           display t-uf at 1120 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-cidade.
           display t-cidade at 1134 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-ddd.
           display t-ddd at 1163 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-cep.
           display t-cep-disp at 1220 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-telefone.
           display t-telefone at 1252 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-data.
           display t-data-disp at 1320 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-nascionalidade.
           display t-nascionalidade at 1352 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-rg.
           display t-rg-disp at 1420 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-emissor.
           display t-emissor at 1452 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-cpf.
           display t-cpf-disp at 1520 with foreground-color 15 
                   background-color 01.
      *
       dsp-t-cargo.
           display t-cargo at 1552 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela (Associados)
      *
       lmp-codigo.
           display limpa-05 at 0720 with foreground-color 15 
                   background-color 01.
      *
       lmp-condicao.
           display limpa-05 at 0744 with foreground-color 15 
                   background-color 01.
      *
       lmp-razao.
           display limpa at 0820 with foreground-color 15 
                   background-color 01.
      *
       lmp-nome.
           display limpa at 0920 with foreground-color 15 
                   background-color 01.
      *
       lmp-endereco.
           display limpa at 1120 with foreground-color 15 
                   background-color 01.
      *
       lmp-uf.
           display limpa-03 at 1220 with foreground-color 15 
                   background-color 01.
      *
       lmp-cidade.
           display limpa-20 at 1233 with foreground-color 15 
                   background-color 01.
      *
       lmp-cep.
           display limpa-12 at 1264 with foreground-color 15 
                   background-color 01.
      *
       lmp-tel-01.
           display limpa-08 at 1320 with foreground-color 15 
                   background-color 01.
      *
       lmp-tel-02.
           display limpa-08 at 1333 with foreground-color 15 
                   background-color 01.
      *
       lmp-telex.
           display limpa-08 at 1363 with foreground-color 15 
                   background-color 01.
      *
       lmp-fax.
           display limpa-08 at 1420 with foreground-color 15 
                   background-color 01.
      *
       lmp-ramal-01.
           display limpa-03 at 1444 with foreground-color 15 
                   background-color 01.
      *
       lmp-ramal-02.
           display limpa-03 at 1448 with foreground-color 15 
                   background-color 01.
      *
       lmp-ddd.
           display limpa-05 at 1463 with foreground-color 15 
                   background-color 01.
      *
       lmp-sindicato.
           display limpa-05 at 1520 with foreground-color 15 
                   background-color 01.
      *
       lmp-snea.
           display limpa-05 at 1544 with foreground-color 15 
                   background-color 01.
      *
       lmp-embratur.
           display limpa-12 at 1563 with foreground-color 15 
                   background-color 01.
      *
       lmp-iata.
           display limpa-12 at 1620 with foreground-color 15 
                   background-color 01.
      *
       lmp-cotal.
           display limpa-12 at 1644 with foreground-color 15 
                   background-color 01.
      *
       lmp-cgc.
           display limpa at 1720 with foreground-color 15 
                   background-color 01.
      *
       lmp-situacao.
           display limpa-03 at 1820 with foreground-color 15 
                   background-color 01.
      *
       lmp-categoria.
           display limpa-30 at 1844 with foreground-color 15 
                   background-color 01.
      *
       lmp-data-alt.
           display limpa-08 at 1920 with foreground-color 15 
                   background-color 01.
      *
       lmp-data-inc.
           display limpa-08 at 1944 with foreground-color 15 
                   background-color 01.
      *
       lmp-data-exc.
           display limpa-08 at 1963 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para dar Display (Titulares)
      *
       lmp-t-titular.
           display limpa-03 at 0920 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-nome.
           display limpa at 0924 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-endereco.
           display limpa at 1020 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-uf.
           display limpa-03 at 1120 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-cidade.
           display limpa-20 at 1134 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-ddd.
           display limpa-05 at 1163 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-cep.
           display limpa-12 at 1220 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-telefone.
           display limpa-12 at 1252 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-data.
           display limpa-12 at 1320 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-nascionalidade.
           display limpa-15 at 1352 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-rg.
           display limpa-12 at 1420 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-emissor.
           display limpa-12 at 1452 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-cpf.
           display limpa-15 at 1520 with foreground-color 15 
                   background-color 01.
      *
       lmp-t-cargo.
           display limpa-15 at 1552 with foreground-color 15 
                   background-color 01.
      *
       sec-inclusao section.
      *
       lab-inc-00-0.
           display tela-limpa-cad.
           perform rot-open-ab01.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-tabl.
           if erro not = 0
              go to lab-inc-fim
           end-if.
      *
       lab-inc-00.
           display tela-07.
           if param-prioridade < 8
              perform sec-consulta
              go to lab-inc-fim
           end-if.
      *
       lab-inc-01.
           display tela-02.
           move 0 to codigo.
           perform lmp-codigo.
           perform acc-codigo.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform lmp-codigo
              perform sec-consulta
              display tela-07
              go to lab-inc-01
           end-if.
           if codigo = 0 
              go to lab-inc-01
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-02.
           display tela-05.
           move spaces to condicao.
           perform lmp-condicao.
           perform acc-condicao.
           if escape-key = 1
              perform lmp-condicao
              go to lab-inc-01
           end-if.
           move condicao to txt.
           perform rot-texto.
           if txt not = "A" and "F"
              go to lab-inc-02
           end-if.
           move txt to condicao ab01-condicao.
           move codigo to ab01-codigo.
           perform rot-le-ab01.
           if erro = 0
              move " Codigo ja cadastrado - Tecle <Enter>" to 
              mensagem
              display tela-erro-cad
              perform rot-keypress
              display tela-limpa-cad
              go to lab-inc-02
           end-if.
           perform dsp-condicao.
           display tela-limpa-cad.
      *
       lab-inc-03.
           move spaces to razao-social.
           perform lmp-razao.
           perform acc-razao.
           if escape-key = 1
              perform lmp-razao
              go to lab-inc-02
           end-if.
           move razao-social to txt razao-social-a.
           perform rot-texto.
           if txt = spaces 
              go to lab-inc-03
           end-if.
           move txt to razao-social.
      *
       lab-inc-04.
           move spaces to nome-fantasia.
           perform lmp-nome.
           perform acc-nome.
           if escape-key = 1
              perform lmp-nome
              go to lab-inc-03
           end-if.
           move nome-fantasia to txt nome-fantasia-a.
           perform rot-texto.
           if txt = spaces 
              move razao-social-a to nome-fantasia nome-fantasia-a
              perform dsp-nome
              move razao-social to nome-fantasia txt
           end-if.
           move txt to nome-fantasia ab01-nome-fantasia.
           move 0 to erro
           perform rot-le-ab01-1.
           if erro = 0 and razao-social = ab01-razao-social
              move " Nome ja cadastrado - Tecle <Enter>" to 
              mensagem
              display tela-erro-cad
              perform rot-keypress
              display tela-limpa-cad
              go to lab-inc-04
           end-if.
      * 
       lab-inc-05.
           move spaces to endereco.
           perform lmp-endereco.
           perform acc-endereco.
           if escape-key = 1
              perform lmp-endereco
              go to lab-inc-04
           end-if.
           if endereco = spaces
              go to lab-inc-05
           end-if.
           move spaces to rotina-uf.
      *
       lab-inc-06.
           display tela-10.
           move rotina-uf to uf.
           perform lmp-uf.
           perform acc-uf.
           if escape-key = 1
              perform lmp-uf
              display tela-limpa-cad
              go to lab-inc-05
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-inc-06
           end-if.           
           move uf to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-06
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
              go to lab-inc-06
           end-if.
           move reg-tabl to reg-wtab03.
           display tela-limpa-cad.
      *
       lab-inc-07.
           move wtab03-capital to cidade.
           perform lmp-cidade.
           perform acc-cidade.
           if escape-key = 1
              perform lmp-cidade
              go to lab-inc-06
           end-if.
           if cidade = spaces
              go to lab-inc-07
           end-if.
      *
       lab-inc-07-00.
           move 0 to cep.
           perform lmp-cep.
           perform acc-cep.
           if escape-key = 1
              perform lmp-cep
              go to lab-inc-07
           end-if.
           if cep = 0
              go to lab-inc-07-00
           end-if.
           move cep to cep-disp.
           perform dsp-cep.
      *
       lab-inc-08.
           move spaces to telefone (01).
           perform lmp-tel-01.
           perform acc-tel-01.
           if escape-key = 1
              perform lmp-tel-01
              go to lab-inc-07-00
           end-if.
           if telefone (01) = spaces
              perform lmp-tel-01
              go to lab-inc-10
           end-if.
      *
       lab-inc-09.
           move spaces to telefone (02).
           perform lmp-tel-02.
           perform acc-tel-02.
           if escape-key = 1
              perform lmp-tel-02
              go to lab-inc-08
           end-if.
      *
       lab-inc-10.
           move spaces to telex.
           perform lmp-telex.
           perform acc-telex.
           if escape-key = 1
              perform lmp-telex
              if telefone (01) = spaces
                 go to lab-inc-08
              else
                 go to lab-inc-09
              end-if
           end-if.
      *
       lab-inc-11.
           move spaces to fax.
           perform lmp-fax.
           perform acc-fax.
           if escape-key = 1
              perform lmp-fax
              go to lab-inc-10
           end-if.
      *
       lab-inc-12.
           move 0 to ramal (01).
           perform lmp-ramal-01.
           perform acc-ramal-01.
           if escape-key = 1 
              perform lmp-ramal-01
              go to lab-inc-11
           end-if.
           if ramal (01) = spaces
              perform lmp-ramal-01
              go to lab-inc-14
           end-if.
      *
       lab-inc-13.
           move 0 to ramal (02).
           perform lmp-ramal-02.
           perform acc-ramal-02.
           if escape-key = 1
              perform lmp-ramal-02
              go to lab-inc-12
           end-if.
           if ramal (02) = spaces
              perform lmp-ramal-02
           end-if.
      *
       lab-inc-14.
           move wtab03-ddd to ddd.
           perform lmp-ddd.
           perform acc-ddd.
           if escape-key = 1
              perform lmp-ddd
              if ramal (01) = spaces
                 go to lab-inc-12
              else
                 go to lab-inc-13
              end-if
           end-if.
           if ddd = 0
              go to lab-inc-14
           end-if.
      *
       lab-inc-15.
           move 0 to sindicato.
           perform lmp-sindicato.
           perform acc-sindicato.
           if escape-key = 1
              perform lmp-sindicato
              go to lab-inc-14
           end-if.
           if sindicato = 0
              perform lmp-sindicato
           end-if.
      *
       lab-inc-16.
           move 0 to snea.
           perform lmp-snea.
           perform acc-snea.
           if escape-key = 1
              perform lmp-snea
              go to lab-inc-15
           end-if
           if snea = 0 
              perform lmp-snea
           end-if.
      *
       lab-inc-17.
           move spaces to embratur.
           perform lmp-embratur.
           perform acc-embratur.
           if escape-key = 1
              perform lmp-embratur
              go to lab-inc-16
           end-if.
      *
       lab-inc-18.
           move spaces to iata.
           perform lmp-iata.
           perform acc-iata.
           if escape-key = 1
              go to lab-inc-17
           end-if.
      *
       lab-inc-19.
           move spaces to cotal.
           perform lmp-cotal.
           perform acc-cotal.
           if escape-key = 1
              perform lmp-cotal
              go to lab-inc-18
           end-if.
      *
       lab-inc-19-00.
           move 0 to cgc.
           perform lmp-cgc.
           perform acc-cgc.
           if escape-key = 1
              perform lmp-cgc
              go to lab-inc-19
           end-if.
           if cgc = 0 
              go to lab-inc-19-00
           end-if.
           move cgc to cgc-disp.
           perform dsp-cgc.
           move 0 to rotina-codigo.
      *
       lab-inc-20.
           display tela-11.
           move 1 to situacao.
           move rotina-codigo to categoria.
           perform lmp-categoria.
           perform acc-categoria.
           if escape-key = 1
              perform lmp-categoria
              display tela-limpa-cad
              go to lab-inc-19-00
           end-if.
           if escape-key = 3
              move 1 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-20
           end-if.
           if categoria = 0
              go to lab-inc-20
           end-if.
           move 01 to wtab01-tipo.
           move categoria to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-categoria
              go to lab-inc-20
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to dcategoria.
           perform dsp-categoria.
           display tela-limpa-cad.
      *
       lab-inc-21.
           move 0 to data-alt.
           perform lmp-data-alt.
           perform acc-data-alt.
           if escape-key = 1
              perform lmp-data-alt
              go to lab-inc-20
           end-if.
           if data-alt = 0
              go to lab-inc-21
           end-if.
           move data-alt to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-inc-21
           end-if.
           move data-disp to data-alt-disp.
           move dias-corr to data-alt
           perform dsp-data-alt.
      *
       lab-inc-22.
           move 0 to data-inc.
           move 0 to data-exc.
           perform lmp-data-inc.
           perform acc-data-inc.
           if escape-key = 1
              perform lmp-data-inc
              go to lab-inc-21
           end-if.
           if data-inc = 0
              go to lab-inc-22
           end-if.
           move data-inc to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-inc-22
           end-if.
           move data-disp to data-inc-disp.
           move dias-corr to data-inc
           perform dsp-data-inc.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-inc-23.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-22
           end-if.
           if resposta = "N"
              perform lmp-codigo thru lmp-data-exc
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-23
              end-if
           end-if.
      *
       lab-inc-24.
           perform rot-move-ab01.
           write reg-ab01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQAB01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           display tela-06.
           move 0 to campo-kbd.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           if kbd-aux = 62
              perform sec-titular
           end-if.
           perform lmp-codigo thru lmp-data-exc.
           go to lab-inc-01.
      *
       lab-inc-fim.
           perform rot-close-tabl.
           perform rot-close-ab01.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           display tela-limpa-cad.
           display tela-08.
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
                            when kbd2 = 82 or 114
                                 display tela-limpa-cad
                                 perform sec-consulta-razao
                                 display tela-03
                            when kbd2 = 78 or 110
                                 display tela-limpa-cad
                                 perform sec-consulta-nome
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
       lab-cns-codigo-00-1.
           display tela-05.
           move spaces to condicao.
           perform lmp-condicao.
           perform acc-condicao.
           if escape-key = 1
              perform lmp-condicao
              go to lab-cns-codigo-00
           end-if.
           move condicao to txt.
           perform rot-texto.
           if txt not = "A" and "F" and spaces
              go to lab-cns-codigo-00-1
           end-if.
           move low-values to ab01-chave.
           move codigo to ab01-codigo.
           move txt to condicao ab01-condicao. 
           display tela-04.
      *
       lab-cns-codigo-00-a.
           start arqab01 key is not less ab01-chave.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or ab01-codigo = codigo and 
              ab01-condicao = condicao
              perform rot-inic-arquivo
              start arqab01 key is not less ab01-chave
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if ab01-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqab01 key is less ab01-chave.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform err-leitura-ab01
              go to lab-cns-codigo-fim
           end-if.
           if ab01-chave = high-values
              perform rot-fim-arquivo
              start arqab01 key is not less ab01-chave
              move 0 to codigo
              move spaces to condicao
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
                            perform sec-titular
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-codigo-03
                    when kbd-aux = 73
                         go to lab-cns-codigo-01
                    when kbd-aux = 71
                         move low-values to ab01-chave
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to ab01-chave
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-codigo thru lmp-data-exc.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-data-exc.
           display tela-limpa.
           exit.
      *
       sec-consulta-razao section.
      *
       lab-cns-razao-00.
           move spaces to razao-social.
           perform lmp-razao.
           perform acc-razao.
           if escape-key = 1
              perform lmp-razao
              go to lab-cns-razao-fim
           end-if.
           display tela-04.
           move razao-social to txt.
           perform rot-texto.
           move low-values to ab01-chave-1.
           move txt to ab01-chave-2.
      *
       lab-cns-razao-00-a.
           start arqab01 key is not less ab01-chave-2.
           go to lab-cns-razao-03.
      *
       lab-cns-razao-01.
           perform rot-le-anterior.
           if erro not = 0 or ab01-codigo = codigo and 
              ab01-condicao = condicao
              perform rot-inic-arquivo
              start arqab01 key is not less ab01-chave-2
              move 1 to erro
              go to lab-cns-razao-05
           end-if.
           if ab01-chave = high-values
              go to lab-cns-razao-01
           end-if.
           go to lab-cns-razao-04.
      *
       lab-cns-razao-02.
           start arqab01 key is less ab01-chave-2.
      *
       lab-cns-razao-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform err-leitura-ab01
              go to lab-cns-razao-fim
           end-if.
           if ab01-chave = high-values
              perform rot-fim-arquivo
              start arqab01 key is not less ab01-chave-2
              move 0 to codigo
              move spaces to condicao
              move 1 to erro
              go to lab-cns-razao-05
           end-if.
      *
       lab-cns-razao-04.
           perform rot-display.
      *
       lab-cns-razao-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-razao-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-razao-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform sec-titular
                            go to lab-cns-razao-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-razao-03
                    when kbd-aux = 73
                         go to lab-cns-razao-01
                    when kbd-aux = 71
                         move low-values to ab01-chave-2
                         go to lab-cns-razao-00-a
                    when kbd-aux = 79
                         move high-values to ab01-chave-2
                         go to lab-cns-razao-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-razao-05
           end-if.
           perform lmp-codigo thru lmp-data-exc.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-razao-00.
      *
       lab-cns-razao-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-data-exc.
           display tela-limpa.
           exit.
      *
       sec-consulta-nome section.
      *
       lab-cns-nome-00.
           move spaces to nome-fantasia.
           perform lmp-nome.
           perform acc-nome.
           if escape-key = 1
              perform lmp-nome
              go to lab-cns-nome-fim
           end-if.
           display tela-04.
           move nome-fantasia to txt.
           perform rot-texto.
           move low-values to ab01-chave-1.
           move txt to ab01-chave-1.
      *
       lab-cns-nome-00-a.
           start arqab01 key is not less ab01-chave-1.
           go to lab-cns-nome-03.
      *
       lab-cns-nome-01.
           perform rot-le-anterior.
           if erro not = 0 or ab01-codigo = codigo and 
              ab01-condicao = condicao
              perform rot-inic-arquivo
              start arqab01 key is not less ab01-chave-1
              move 1 to erro
              go to lab-cns-nome-05
           end-if.
           if ab01-chave = high-values
              go to lab-cns-nome-01
           end-if.
           go to lab-cns-nome-04.
      *
       lab-cns-nome-02.
           start arqab01 key is less ab01-chave-1.
      *
       lab-cns-nome-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform err-leitura-ab01
              go to lab-cns-nome-fim
           end-if.
           if ab01-chave = high-values
              perform rot-fim-arquivo
              start arqab01 key is not less ab01-chave-1
              move 0 to codigo
              move spaces to condicao
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
                            perform sec-titular
                            go to lab-cns-nome-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-nome-03
                    when kbd-aux = 73
                         go to lab-cns-nome-01
                    when kbd-aux = 71
                         move low-values to ab01-chave-1
                         go to lab-cns-nome-00-a
                    when kbd-aux = 79
                         move high-values to ab01-chave-1
                         go to lab-cns-nome-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-nome-05
           end-if.
           perform lmp-codigo thru lmp-data-exc.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-cns-nome-00.
      *
       lab-cns-nome-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-data-exc.
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
           perform rot-open-ab02.
           if erro not = 0
              go to lab-exc-fim
           end-if.
           perform rot-open-ab04.
           if erro not = 0
              go to lab-exc-fim
           end-if.
      *
       lab-exc-00.
           perform rot-le-ab01-lock.
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
           move ab01-codigo to ab02-codigo.
           start arqab02 key is equal ab02-chave invalid key
                 go to lab-exc-02
           end-start.
           perform rot-le-ab02-lock.
           delete arqab02 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQAB02A.DAT - Tecle <Enter>
      -           " " to mensagem
                  display tela-erro
                  perform rot-keypress
                  display tela-limpa
                  go to lab-exc-fim
           end-delete.
      *
       lab-exc-02.
           move ab01-codigo to ab04-codigo.
           move ab01-condicao to ab04-condicao.
           move 0 to ab04-titular.
           start arqab04 key is not less ab04-chave invalid key
                 move 1 to erro
                 perform err-leitura-ab04
           end-start.
           if erro not = 0
              go to lab-exc-03
           end-if.
           perform rot-le-ab04-lock.
           if ab04-codigo not = ab01-codigo or
              ab04-condicao not = ab01-condicao
              unlock arqab04 record
              go to lab-exc-03
           end-if.
           delete arqab04 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQAB04A.DAT - Tecle <Enter>
      -           " " to mensagem
                  display tela-erro
                  perform rot-keypress
                  display tela-limpa
                  go to lab-exc-fim
           end-delete.
           go to lab-exc-02.
      *
       lab-exc-03.
           delete arqab01 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQAB01A.DAT - Tecle <Enter>
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
           unlock arqab01 record.
           perform rot-close-ab04.
           perform rot-close-ab02.
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
           perform rot-open-ab02.
           if erro not = 0
              go to lab-alt-fim
           end-if.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-alt-fim
           end-if.
           perform rot-le-ab01-lock.
           perform rot-display.
      *
       lab-alt-01.
           perform acc-razao.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           move razao-social to txt razao-social-a.
           perform rot-texto.
           if txt = spaces 
              go to lab-alt-01
           end-if.
           move txt to razao-social.
      *
       lab-alt-02.
           perform acc-nome.
           if escape-key = 1
              move razao-social-a to razao-social
              go to lab-alt-01
           end-if.
           move nome-fantasia to txt nome-fantasia-a.
           perform rot-texto.
           if txt = spaces 
              move razao-social-a to nome-fantasia nome-fantasia-a
              perform dsp-nome
              move razao-social to nome-fantasia txt
           end-if.
           move txt to nome-fantasia ab01-nome-fantasia.
           move 0 to erro
           perform rot-le-ab01-1.
           if erro = 0 and razao-social = ab01-razao-social and 
              codigo not = ab01-codigo or condicao not = ab01-condicao
              move " Nome ja cadastrado - Tecle <Enter>" to 
              mensagem
              display tela-erro-cad
              perform rot-keypress
              display tela-limpa-cad
              move nome-fantasia-a to nome-fantasia
              go to lab-alt-02
           end-if.
      * 
       lab-alt-03.
           perform acc-endereco.
           if escape-key = 1
              move nome-fantasia-a to nome-fantasia
              go to lab-alt-02
           end-if.
           if endereco = spaces
              go to lab-alt-03
           end-if.
           move uf to rotina-uf.
      *
       lab-alt-04.
           display tela-10.
           move rotina-uf to uf.
           perform acc-uf.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-03
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-alt-04
           end-if.
           move uf to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-04
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
              go to lab-alt-04
           end-if.
           move reg-tabl to reg-wtab03.
           display tela-limpa-cad.
      *
       lab-alt-05.
           perform acc-cidade.
           if escape-key = 1
              go to lab-alt-04
           end-if.
           if cidade = spaces
              go to lab-alt-05
           end-if.
      *
       lab-alt-05-00.
           perform lmp-cep.
           perform acc-cep.
           if escape-key = 1
              move cep to cep-disp 
              perform dsp-cep
              go to lab-alt-05
           end-if.
           if cep = 0
              go to lab-alt-05-00
           end-if.
           move cep to cep-disp.
           perform dsp-cep.
      *
       lab-alt-06.
           perform acc-tel-01.
           if escape-key = 1
              go to lab-alt-05-00
           end-if.
           if telefone (01) = spaces
              go to lab-alt-08
           end-if.
      *
       lab-alt-07.
           perform acc-tel-02.
           if escape-key = 1
              go to lab-alt-06
           end-if.
      *
       lab-alt-08.
           perform acc-telex.
           if escape-key = 1
              if telefone (01) = spaces
                 go to lab-alt-06
              else
                 go to lab-alt-07
              end-if
           end-if.
      *
       lab-alt-09.
           perform acc-fax.
           if escape-key = 1
              go to lab-alt-08
           end-if.
      *
       lab-alt-10.
           perform acc-ramal-01.
           if escape-key = 1 
              if ramal (01) = spaces 
                 perform lmp-ramal-01
              end-if
              go to lab-alt-09
           end-if.
           if ramal (01) = spaces
              perform lmp-ramal-01 thru lmp-ramal-02
              go to lab-alt-12
           end-if.
      *
       lab-alt-11.
           perform acc-ramal-02.
           if escape-key = 1
              go to lab-alt-10
           end-if.
           if ramal (02) = spaces 
              perform lmp-ramal-02
           end-if.
      *
       lab-alt-12.
           perform acc-ddd.
           if escape-key = 1
              if ramal (01) = spaces
                 go to lab-alt-10
              else
                 go to lab-alt-11
              end-if
           end-if.
           if ddd = 0
              go to lab-alt-12
           end-if.
      *
       lab-alt-13.
           perform acc-sindicato.
           if escape-key = 1
              if sindicato = 0 
                 perform lmp-sindicato
              end-if
              go to lab-alt-12
           end-if.
           if sindicato = 0
              perform lmp-sindicato
           end-if.
      *
       lab-alt-14.
           perform acc-snea.
           if escape-key = 1
              if snea = 0 
                 perform lmp-snea
              end-if
              go to lab-alt-13
           end-if
           if snea = 0 
              perform lmp-snea
           end-if.
      *
       lab-alt-15.
           perform acc-embratur.
           if escape-key = 1
              go to lab-alt-14
           end-if.
      *
       lab-alt-16.
           perform acc-iata.
           if escape-key = 1
              go to lab-alt-15
           end-if.
      *
       lab-alt-17.
           perform acc-cotal.
           if escape-key = 1
              go to lab-alt-16
           end-if.
      *
       lab-alt-17-00.
           perform acc-cgc.
           if escape-key = 1
              move cgc to cgc-disp
              perform dsp-cgc
              go to lab-alt-17
           end-if.
           if cgc = 0
              go to lab-alt-17-00
           end-if.
           move cgc to cgc-disp.
           perform dsp-cgc.
      *
       lab-alt-18.
           display tela-09.
           perform acc-situacao.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-17-00
           end-if.
           if situacao not = 1 and 2 and 3 and 4 and 5
              go to lab-alt-18
           end-if.
           if situacao = 1
              move 0 to data-exc
              perform lmp-data-exc
           end-if.
           display tela-limpa-cad.
           move categoria to rotina-codigo.
      *
       lab-alt-19.
           display tela-11.
           move rotina-codigo to categoria.
           perform lmp-categoria.
           perform acc-categoria.
           if escape-key = 1
              perform dsp-categoria
              display tela-limpa-cad
              go to lab-alt-18
           end-if.
           if escape-key = 3
              move 1 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-19
           end-if.
           if categoria = 0
              go to lab-alt-19
           end-if.
           move 01 to wtab01-tipo.
           move categoria to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-categoria
              go to lab-alt-19
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to dcategoria.
           perform dsp-categoria.
           display tela-limpa-cad.
      *
       lab-alt-20.
           perform lmp-data-alt.
           perform acc-data-alt.
           if escape-key = 1
              perform dsp-data-alt
              go to lab-alt-19
           end-if.
           if data-alt = 0
              go to lab-alt-20
           end-if.
           move data-alt to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              move ab01-data-alt to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-alt
              go to lab-alt-20
           end-if.
           move data-disp to data-alt-disp.
           move dias-corr to data-alt
           perform dsp-data-alt.
      *
       lab-alt-21.
           perform lmp-data-inc.
           perform acc-data-inc.
           if escape-key = 1
              perform dsp-data-inc
              move data-alt to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-alt
              go to lab-alt-20
           end-if.
           if data-inc = 0
              go to lab-alt-21
           end-if.
           move data-inc to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              move ab01-data-inc to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-inc
              go to lab-alt-21
           end-if.
           move data-disp to data-inc-disp.
           move dias-corr to data-inc
           perform dsp-data-inc.
           if situacao = 1
              perform lmp-data-exc
              go to lab-alt-23
           end-if.
      *
       lab-alt-22.
           perform lmp-data-exc.
           perform acc-data-exc.
           if escape-key = 1
              perform dsp-data-exc
              move data-inc to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-inc
              go to lab-alt-21
           end-if.
           if data-exc = 0
              go to lab-alt-22
           end-if.
           move data-exc to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              move ab01-data-exc to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to data-exc
              go to lab-alt-21
           end-if.
           move data-disp to data-exc-disp.
           move dias-corr to data-exc
           perform dsp-data-exc.
      *
       lab-alt-23.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              if situacao = 1
                 move data-inc to dias-corr
                 move 1 to opcao-data
                 perform rot-data
                 move dia-euro to dia-aux
                 move mes-euro to mes-aux
                 move ano-euro to ano-aux
                 move data-aux to data-inc
                 go to lab-alt-21
             else
                 move data-exc to dias-corr
                 move 1 to opcao-data
                 perform rot-data
                 move dia-euro to dia-aux
                 move mes-euro to mes-aux
                 move ano-euro to ano-aux
                 move data-aux to data-exc
                 go to lab-alt-22
             end-if 
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-23
              end-if
           end-if.
           perform rot-move-ab01.
           rewrite reg-ab01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQAB01A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-alt-fim
           end-rewrite.
           if ab01-condicao = "A"
              perform rot-rewrite-ab02
           end-if.
      *
       lab-alt-25.
           move "Registro alterado - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       lab-alt-fim.
           unlock arqab01 record.
           perform rot-close-ab02.
           display tela-04.
           exit.
      *
       sec-titular section.
      *
       lab-tit-00.
           move 0 to box-col box-lin.
           move 79 to box-col-f.
           move 24 to box-lin-f.
           perform rot-save-buffer-01.
           move 06 to box-col.
           move 06 to box-lin.
           move 70 to box-col-f.
           move 17 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-12.
      *
       lab-tit-01.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-tit-fim
           end-if.
           perform rot-le-ab01-lock.
           perform sec-inclusao-tit.
           unlock arqab01 record.
           move 0 to box-col box-lin.
           move 79 to box-col-f.
           move 24 to box-lin-f.
           perform rot-rest-buffer-01.
      *
       lab-tit-fim.
           exit.
      *
       sec-inclusao-tit section.
      *
       lab-inc-tit-00.
           display tela-limpa-cad-t.
           perform rot-open-ab04.
           if erro not = 0
              go to lab-inc-tit-fim
           end-if.
           if param-prioridade < 1
              perform sec-consulta-tit
              go to lab-inc-tit-fim
           end-if.
           display tela-13.
      *
       lab-inc-tit-01.
           display tela-15.
           move 0 to t-titular.
           perform lmp-t-titular.
           perform acc-t-titular.
           if escape-key = 1
              go to lab-inc-tit-fim
           end-if.
           if escape-key = 3
              perform lmp-t-titular
              perform sec-consulta-tit
              display tela-13
              go to lab-inc-tit-01
           end-if.
           if t-titular = 0
              go to lab-inc-tit-01
           end-if.
           move ab01-codigo to ab04-codigo.
           move ab01-condicao to ab04-condicao .
           move t-titular to ab04-titular.
           perform rot-le-ab04.
           if erro = 0
              perform err-tit-c
              go to lab-inc-tit-01
           end-if.
           display tela-limpa-cad-t.
      *
       lab-inc-tit-02.
           move spaces to t-nome.
           perform lmp-t-nome.
           perform acc-t-nome.
           if escape-key = 1
              perform lmp-t-nome
              go to lab-inc-tit-01
           end-if.
           move t-nome to txt t-nome-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-tit-02
           end-if.
           move txt to ab04-nome t-nome.
           move 0 to t-titular-aux.
           perform sec-acha-titular.
           if erro = 0
              perform err-tit-c
              go to lab-inc-tit-02
           end-if.
      *
       lab-inc-tit-03.
           move spaces to t-endereco.
           perform lmp-t-endereco.
           perform acc-t-endereco.
           if escape-key = 1
              perform lmp-t-endereco
              go to lab-inc-tit-02
           end-if.
           if t-endereco = spaces
              go to lab-inc-tit-03
           end-if.
           move spaces to rotina-uf.
      *
       lab-inc-tit-04.
           display tela-16.
           move rotina-uf to t-uf.
           perform lmp-t-uf.
           perform acc-t-uf.
           if escape-key = 1
              perform lmp-t-uf
              display tela-limpa-cad-t
              go to lab-inc-tit-03
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-inc-tit-04
           end-if.
           move t-uf to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-tit-04
           end-if.
           move txt to t-uf.
           perform dsp-t-uf.
           move 03 to wtab03-tipo.
           move t-uf to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado-t
              go to lab-inc-tit-04
           end-if.
           move reg-tabl to reg-wtab03.
           display tela-limpa-cad-t.
      *
       lab-inc-tit-05.
           move wtab03-capital to t-cidade.
           perform lmp-t-cidade.
           perform acc-t-cidade.
           if escape-key = 1
              perform lmp-t-cidade
              go to lab-inc-tit-04
           end-if.
           if t-cidade = spaces
              go to lab-inc-tit-05
           end-if.
      *
       lab-inc-tit-06.
           move wtab03-ddd to t-ddd.
           perform lmp-t-ddd.
           perform acc-t-ddd.
           if escape-key = 1
              perform lmp-t-ddd
              go to lab-inc-tit-05
           end-if.
           if t-ddd = 0
              go to lab-inc-tit-06
           end-if.
      *
       lab-inc-tit-07.
           move 0 to t-cep.
           perform lmp-t-cep.
           perform acc-t-cep.
           if escape-key = 1
              perform lmp-t-cep
              go to lab-inc-tit-06
           end-if.
           if t-cep = 0
              go to lab-inc-tit-07
           end-if.
           move t-cep to t-cep-disp.
           perform dsp-t-cep.
      *
       lab-inc-tit-08.
           move spaces to t-telefone.
           perform lmp-t-telefone.
           perform acc-t-telefone.
           if escape-key = 1
              perform lmp-t-telefone
              go to lab-inc-tit-07
           end-if.
      *
       lab-inc-tit-09.
           move 0 to t-data.
           perform lmp-t-data. 
           perform acc-t-data.
           if escape-key = 1
              perform lmp-t-data
              go to lab-inc-tit-08
           end-if.
           if t-data = 0
              perform lmp-t-data
              go to lab-inc-tit-10
           end-if.
           move t-data to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i-t
              go to lab-inc-tit-09
           end-if.
           move data-disp to t-data-disp.
           move dias-corr to t-data.
           perform dsp-t-data.
      *
       lab-inc-tit-10.
           move spaces to t-nascionalidade.
           perform lmp-t-nascionalidade.
           perform acc-t-nascionalidade.
           if escape-key = 1
              perform lmp-t-nascionalidade
              go to lab-inc-tit-09
           end-if.
           perform dsp-t-nascionalidade.
      *
       lab-inc-tit-11.
           move 0 to t-rg.
           perform lmp-t-rg.
           perform acc-t-rg.
           if escape-key = 1
              perform lmp-t-rg
              go to lab-inc-tit-10
           end-if.
           if t-rg = 0
              perform lmp-t-rg
              go to lab-inc-tit-12
           end-if.
           move t-rg to t-rg-disp.
           perform dsp-t-rg.
      *
       lab-inc-tit-12.
           move spaces to t-emissor.
           perform lmp-t-emissor.
           perform acc-t-emissor.
           if escape-key = 1
              perform lmp-t-emissor
              go to lab-inc-tit-11
           end-if.
      *
       lab-inc-tit-13.
           move 0 to t-cpf.
           perform lmp-t-cpf.
           perform acc-t-cpf.
           if escape-key = 1
              perform lmp-t-cpf
              go to lab-inc-tit-12
           end-if.
           if t-cpf = 0
              perform lmp-t-cpf
              go to lab-inc-tit-14
           end-if.
           move t-cpf to t-cpf-disp.
           perform dsp-t-cpf.
      *
       lab-inc-tit-14.
           move spaces to t-cargo
           perform lmp-t-cargo.
           perform acc-t-cargo.
           if escape-key = 1
              perform lmp-t-cargo
              go to lab-inc-tit-13
           end-if.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad-t.
      *
       lab-inc-tit-15.
           perform accept-resposta-cad-t.
           if escape-key = 1
              display tela-limpa-cad-t
              go to lab-inc-tit-14
           end-if.
           if resposta = "N"
              perform lmp-t-titular thru lmp-t-cargo
              go to lab-inc-tit-01
           else
              if resposta not = "S"
                 go to lab-inc-tit-15
              end-if
           end-if.
      *
       lab-inc-tit-16.
           perform rot-move-ab04.
           write reg-ab04 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQAB04A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-tit-fim
           end-write.
           display tela-17.
           perform rot-keypress.
           perform lmp-t-titular thru lmp-t-cargo.
           go to lab-inc-tit-01.
      *
       lab-inc-tit-fim.
           perform rot-close-ab04.
           exit.
      *
       sec-consulta-tit section.
      *
       lab-cns-tit-00.
           display tela-limpa-cad-t.
           display tela-14.
      *
       lab-cns-tit-01.
           display tela-18.
           move 0 to kbd2.
           perform until kbd2 = 27
                   perform rot-keypress
                   evaluate true
                            when kbd2 = 84 or 116
                                 display tela-limpa-cad-t
                                 perform sec-consulta-titular
                                 display tela-18
                            when kbd2 = 78 or 110
                                 display tela-limpa-cad-t
                                 perform sec-consulta-nome-tit
                                 display tela-18
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-tit-fim.
           display tela-limpa.
           exit.
      *
       sec-consulta-titular section.
      *
       lab-cns-tit-00.
           move 0 to t-titular.
           perform lmp-t-titular.
           perform acc-t-titular.
           if escape-key = 1
              perform lmp-t-titular
              go to lab-cns-tit-fim
           end-if.
           display tela-19.
           move ab01-codigo to ab04-codigo.
           move ab01-condicao to ab04-condicao.
           move t-titular to ab04-titular.
      *
       lab-cns-tit-00-a.
           start arqab04 key is not less ab04-chave.
           go to lab-cns-tit-03.
      *
       lab-cns-tit-01.
           perform rot-le-ab04-prev.
           if erro not = 0 or 
              ab04-codigo not = ab01-codigo or
              ab04-condicao not = ab01-condicao or t-flag = 2
              perform rot-inic-arquivo-t
              move ab01-codigo to ab04-codigo
              move ab01-condicao to ab04-condicao
              move 0 to ab04-titular
              start arqab04 key is not less ab04-chave
              move 2 to t-flag
              move 1 to erro
              go to lab-cns-tit-05
           end-if.
           if ab04-chave = high-values
              go to lab-cns-tit-01
           end-if.
           go to lab-cns-tit-04.
      *
       lab-cns-tit-02.
           start arqab04 key is less ab04-chave.
      *
       lab-cns-tit-03.
           perform rot-le-ab04-next.
           if erro not = 0
              perform err-leitura-ab04
              go to lab-cns-tit-fim
           end-if.
           if ab04-chave = high-values or 
              ab04-codigo not = ab01-codigo or
              ab04-condicao not = ab01-condicao or t-flag = 1
              move ab01-codigo to ab04-codigo
              move ab01-condicao to ab04-condicao
              move 99 to ab04-titular
              perform rot-fim-arquivo-t
              start arqab04 key is less ab04-chave
              move 1 to erro
              move 1 to t-flag
              go to lab-cns-tit-05
           end-if.
      *
       lab-cns-tit-04.
           perform rot-display-tit.
           move 0 to t-flag.
      *
       lab-cns-tit-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao-tit
                            go to lab-cns-tit-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao-tit
                            go to lab-cns-tit-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-tit-03
                    when kbd-aux = 73
                         go to lab-cns-tit-01
                    when kbd-aux = 71
                         move ab01-codigo to ab04-codigo
                         move ab01-condicao to ab04-condicao
                         move 0 to ab04-titular
                         move 0 to t-flag
                         go to lab-cns-tit-00-a
                    when kbd-aux = 79
                         move ab01-codigo to ab04-codigo
                         move ab01-condicao to ab04-condicao
                         move 99 to ab04-titular
                         move 0 to t-flag
                         go to lab-cns-tit-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-tit-05
           end-if.
           perform lmp-t-titular thru lmp-t-cargo.
           display tela-limpa-cad-t.
           display tela-limpa.
           go to lab-cns-tit-00.
      *
       lab-cns-tit-fim.
           move zeros to campo-kbd.
           perform lmp-t-titular thru lmp-t-cargo.
           display tela-limpa.
           exit.
      *
       sec-consulta-nome-tit section.
      *
       lab-cns-nome-tit-00.
           move spaces to t-nome.
           perform lmp-t-nome.
           perform acc-t-nome.
           if escape-key = 1
              perform lmp-t-nome
              go to lab-cns-nome-tit-fim
           end-if.
           display tela-19.
           move t-nome to txt.
           perform rot-texto.
           move txt to ab04-chave-1.
      *
       lab-cns-nome-tit-00-a.
           start arqab04 key is not less ab04-chave-1.
           go to lab-cns-nome-tit-03.
      *
       lab-cns-nome-tit-01.
           perform rot-le-ab04-prev.
           if erro not = 0 or 
              ab04-controle = t-controle 
              perform rot-inic-arquivo-t
              start arqab04 key is not less ab04-chave-1
              move 1 to erro
              go to lab-cns-nome-tit-05
           end-if.
           if ab04-chave = high-values
              go to lab-cns-nome-tit-01
           end-if.
           move ab04-controle to t-controle.
           go to lab-cns-nome-tit-04.
      *
       lab-cns-nome-tit-02.
           start arqab04 key is less ab04-chave-1.
      *
       lab-cns-nome-tit-03.
           move 0 to erro.
           perform rot-le-ab04-next.
           if erro not = 0
              perform err-leitura-ab04
              go to lab-cns-nome-tit-fim
           end-if.
           if ab04-chave = high-values
              perform rot-fim-arquivo-t
              start arqab04 key is not less ab04-chave-1
              move 1 to erro
              go to lab-cns-nome-tit-05
           end-if.
      *
       lab-cns-nome-tit-04.
           unlock arqab01 record.
           move ab04-codigo to ab01-codigo.
           move ab04-condicao to ab01-condicao.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-cns-nome-tit-fim
           end-if.
           perform rot-le-ab01-lock.
           perform rot-display-tit.
      *
       lab-cns-nome-tit-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao-tit
                            go to lab-cns-nome-tit-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao-tit
                            go to lab-cns-nome-tit-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-nome-tit-03
                    when kbd-aux = 73
                         go to lab-cns-nome-tit-01
                    when kbd-aux = 71
                         move low-values to ab04-chave-1
                         go to lab-cns-nome-tit-00-a
                    when kbd-aux = 79
                         move high-values to ab04-chave-1
                         go to lab-cns-nome-tit-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-nome-tit-05
           end-if.
           perform lmp-t-titular thru lmp-t-cargo.
           display tela-limpa-cad-t.
           display tela-limpa.
           go to lab-cns-nome-tit-00.
      *
       lab-cns-nome-tit-fim.
           move zeros to campo-kbd.
           perform lmp-t-titular thru lmp-t-cargo.
           display tela-limpa.
           exit.
      *
       sec-exclusao-tit section.
      *
       lab-exc-tit-00-0.
           display tela-limpa-cad-t.
           if param-prioridade < 7
              perform display-erro-usr-t
              go to lab-exc-tit-fim
           end-if.
           perform rot-ponteiro-ab04.
           if erro not = 0
              go to lab-exc-tit-fim
           end-if.
      *
       lab-exc-tit-00.
           perform rot-le-ab04-lock.
           perform rot-display-tit.
           move "Excluir (S) (N) ?" to mensagem.
           display tela-mensagem-cad-t.
      *
       lab-exc-tit-01.
           perform accept-resposta-cad-t.
           if escape-key = 1
              display tela-limpa-cad-t
              go to lab-exc-tit-fim
           end-if.
           if resposta = "N"
              go to lab-exc-tit-fim
           else
              if resposta not = "S"
                 go to lab-exc-tit-01
              end-if
           end-if.
           delete arqab04 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQAB04A.DAT - Tecle <Enter>
      -           " " to mensagem
                  display tela-erro
                  perform rot-keypress
                  display tela-limpa
                  go to lab-exc-tit-fim
           end-delete.
           move "Registro excluido - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad-t.
           perform rot-keypress.
           display tela-limpa-cad-t.
      *
       lab-exc-tit-fim.
           unlock arqab04 record.
           display tela-19.
           exit.
      *
       sec-alteracao-tit section.
      *
       lab-alt-00-0.
           display tela-limpa-cad-t.
           if param-prioridade < 5
              perform display-erro-usr-t
              go to lab-alt-tit-fim
           end-if.
           perform rot-ponteiro-ab04.
           if erro not = 0
              go to lab-alt-tit-fim
           end-if.
           perform rot-le-ab04-lock.
           perform rot-display-tit.
           move t-titular to t-titular-aux.
      *
       lab-alt-tit-01.
           perform lmp-t-titular.
           perform acc-t-titular.
           if escape-key = 1
              go to lab-alt-tit-fim
           end-if.
           if t-titular = 0
              go to lab-alt-tit-01
           end-if.
           if t-titular not equal t-titular-aux
              move ab01-codigo to ab04-codigo
              move ab01-condicao to ab04-condicao
              move t-titular to ab04-titular
              perform rot-le-ab04
              if erro = 0
                 perform err-tit-c
                 go to lab-alt-tit-01
              end-if
           end-if.
      *
       lab-alt-tit-02.
           perform acc-t-nome.
           if escape-key = 1
              go to lab-alt-tit-01
           end-if.
           move t-nome to txt t-nome-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-tit-02
           end-if.
           move txt to ab04-nome t-nome.
           perform sec-acha-titular.
           if erro = 0 
              perform err-tit-c
              go to lab-alt-tit-02
           end-if.
      *
       lab-alt-tit-03.
           perform acc-t-endereco.
           if escape-key = 1
              go to lab-alt-tit-02
           end-if.
           if t-endereco = spaces
              go to lab-alt-tit-03
           end-if.
           move t-uf to rotina-uf.
      *
       lab-alt-tit-04.
           display tela-16.
           move rotina-uf to t-uf.
           perform acc-t-uf.
           if escape-key = 1
              display tela-limpa-cad-t
              go to lab-alt-tit-03
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-alt-tit-04
           end-if.
           move t-uf to txt.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-tit-04
           end-if.
           move txt to t-uf.
           perform dsp-t-uf.
           move 03 to wtab03-tipo.
           move t-uf to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-estado-t
              go to lab-alt-tit-04
           end-if.
           display tela-limpa-cad-t.
      *
       lab-alt-tit-05.
           perform acc-t-cidade.
           if escape-key = 1
              go to lab-alt-tit-04
           end-if.
           if t-cidade = spaces
              go to lab-alt-tit-05
           end-if.
      *
       lab-alt-tit-06.
           perform acc-t-ddd.
           if escape-key = 1
              go to lab-alt-tit-05
           end-if.
           if t-ddd = 0
              go to lab-alt-tit-06
           end-if.
      *
       lab-alt-tit-07.
           perform lmp-t-cep.
           perform acc-t-cep.
           if escape-key = 1
              move t-cep to t-cep-disp
              perform dsp-t-cep
              go to lab-alt-tit-06
           end-if.
           if t-cep = 0
              go to lab-alt-tit-07
           end-if.
           move t-cep to t-cep-disp.
           perform dsp-t-cep.
      *
       lab-alt-tit-08.
           perform acc-t-telefone.
           if escape-key = 1
              go to lab-alt-tit-07
           end-if.
      *
       lab-alt-tit-09.
           perform lmp-t-data.
           perform acc-t-data.
           if escape-key = 1
              perform dsp-t-data
              go to lab-alt-tit-08
           end-if.
           if t-data = 0
              perform lmp-t-data
              go to lab-alt-tit-10
           end-if.
           move t-data to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i-t
              move ab04-data-nasc to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to t-data
              go to lab-alt-tit-09
           end-if.
           move data-disp to t-data-disp.
           move dias-corr to t-data.
           perform dsp-t-data.
      *
       lab-alt-tit-10.
           perform acc-t-nascionalidade.
           if escape-key = 1
              go to lab-alt-tit-09
           end-if.
           perform dsp-t-nascionalidade.
      *
       lab-alt-tit-11.
           perform lmp-t-rg.
           perform acc-t-rg.
           if escape-key = 1
              move t-rg to t-rg-disp
              perform dsp-t-rg
              move t-data to dias-corr
              move 1 to opcao-data
              perform rot-data
              move dia-euro to dia-aux
              move mes-euro to mes-aux
              move ano-euro to ano-aux
              move data-aux to t-data
              go to lab-alt-tit-10
           end-if.
           if t-rg = 0
              perform lmp-t-rg
              go to lab-alt-tit-12
           end-if.
           move t-rg to t-rg-disp.
           perform dsp-t-rg.
      *
       lab-alt-tit-12.
           perform acc-t-emissor.
           if escape-key = 1
              go to lab-alt-tit-11
           end-if.
      *
       lab-alt-tit-13.
           perform lmp-t-cpf.
           perform acc-t-cpf.
           if escape-key = 1
              move t-cpf to t-cpf-disp
              perform dsp-t-cpf
              go to lab-alt-tit-12
           end-if.
           if t-cpf = 0
              perform lmp-t-cpf
              go to lab-alt-tit-14
           end-if.
           move t-cpf to t-cpf-disp.
           perform dsp-t-cpf.
      *
       lab-alt-tit-14.
           perform acc-t-cargo.
           if escape-key = 1
              go to lab-alt-tit-13
           end-if.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad-t.
      *
       lab-alt-tit-15.
           perform accept-resposta-cad-t.
           if escape-key = 1
              display tela-limpa-cad-t
              go to lab-alt-tit-14
           end-if.
           if resposta = "N"
              go to lab-alt-tit-fim
           else
              if resposta not = "S"
                 go to lab-alt-tit-15
              end-if
           end-if.
      *
       lab-alt-tit-16.
           move ab01-codigo to ab04-codigo.
           move ab01-condicao to ab04-condicao.
           move t-titular-aux to ab04-titular.
           perform rot-ponteiro-ab04.
           if erro not = 0
              go to lab-alt-tit-fim
           end-if.
           perform rot-le-ab04-lock.
           if t-titular not = t-titular-aux
              delete arqab04 invalid key 
                     move 1 to erro
                     move " Erro de exclusao - ARQAB04A.DAT - Tecle <Ent
      -              "er>" to mensagem
                     display tela-erro
                     perform rot-keypress
                     display tela-limpa
                     go to lab-alt-tit-fim
              end-delete
              perform rot-move-ab04
              write reg-ab04 invalid key 
                    move 1 to erro
                    move " Erro de gravacao - ARQAB04A.DAT - Tecle <Ente
      -             "r>" to mensagem
                    display tela-erro
                    perform rot-keypress
                    display tela-limpa
                    go to lab-alt-tit-fim
              end-write
           else
              perform rot-move-ab04
              rewrite reg-ab04 invalid key 
                      move 1 to erro
                      move " Erro de regravacao - ARQAB04A.DAT - Tecle <
      -               "Enter>" to mensagem
                      display tela-erro
                      perform rot-keypress
                      display tela-limpa
                      go to lab-alt-tit-fim
              end-rewrite
           end-if.
           move "Registro alterado - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad-t.
           perform rot-keypress.
      *
       lab-alt-tit-fim.
           unlock arqab04 record.
           display tela-19.
           exit.