      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGAB0B       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao e Consulta de TKT's :                              *
      *                                                             *
      *  Data da ultima alteracao:    29/08/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgab0b.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
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
           select arqab05 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ab05-chave
                  alternate record key is ab05-chave-1 with duplicates
                  alternate record key is ab05-chave-2 with duplicates
                  alternate record key is ab05-chave-3 with duplicates
                  alternate record key is ab05-chave-4 with duplicates
                  alternate record key is ab05-chave-5 with duplicates
                  alternate record key is ab05-chave-6 with duplicates
                  alternate record key is ab05-chave-7 with duplicates
                  alternate record key is ab05-chave-8 with duplicates
                  alternate record key is ab05-chave-9 with duplicates
                  alternate record key is ab05-chave-10 with duplicates
                  file status is ab05-status.
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
       copy fdab02.lib.
      *    
       copy fdab05.lib.
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
       01 ab05-status                  pic x(02) value "00".
       01 ab05-stat                    pic x(01) value "F".
      *
       01 nome-arq-ab05.
          02 ab05-dir                  pic x(03) value "AB3".
          02 filler                    pic x(01) value "\".
          02 ab05-nome                 pic x(08) value "ARQAB05A".
          02 filler                    pic x(01) value ".".
          02 ab05-ext                  pic x(03) value "DAT".
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
          02 cb-programa               pic x(08) value "PGAB0B".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(45) value spaces.
       01 limpa-08                     pic x(08) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 sequencia                    pic 9(01) value 0.
       01 linha                        pic 9(03) comp-5 value 0.
       01 coluna                       pic 9(04) comp-5 value 1.
       01 tamanho                      pic 9(04) comp-5 value 78.
       01 pagina                       pic 9(03) value 0.
       01 linha-detalhe                pic x(78) value spaces.
       01 tracos-c                     pic x(78) value all "Ä".
       01 tracos-i                     pic x(80) value all "-".
       01 sub-total                    pic 9(06) value 0.
       01 total                        pic 9(06) value 0.
       01 cons-ant                     pic x(05) value spaces.
      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
          02 sele-cons                 pic 9(05) value 0.
          02 sele-cons-disp            pic x(05) value spaces.
          02 sele-dcons                pic x(40) value spaces.
          02 sele-data-rec-i           pic 9(06) value 0.
          02 sele-data-rec-f           pic 9(06) value 0.
          02 sele-data-disp            pic x(08) value spaces.
          02 sele-destino              pic x(03) value spaces.
          02 sele-destino-disp         pic x(05) value spaces.
          02 sele-data-emb-i           pic 9(06) value 0.
          02 sele-data-emb-f           pic 9(06) value 0.
          02 sele-data-ent-i           pic 9(06) value 0.
          02 sele-data-ent-f           pic 9(06) value 0.
          02 sele-horario-e            pic 9(04) value 0.
          02 sele-horario-e-disp       pic x(05) value spaces.
          02 sele-horario-e-aux        pic 99b99 value 0.
          02 sele-voo                  pic 9(04) value 0.
          02 sele-voo-disp             pic x(05) value spaces.
          02 sele-cia                  pic x(02) value spaces.
          02 sele-cia-disp             pic x(05) value spaces.
          02 sele-dcia                 pic x(17) value spaces.
          02 sele-aerop                pic 9(01) value 0.
          02 sele-aerop-disp           pic x(05) value spaces.
          02 sele-texto                pic x(40) value spaces.
          02 sele-device               pic 9(01) value 0.
      *
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 horario-r.
          02 hora-r                    pic 9(02) value 0.
          02 min-r                     pic 9(02) value 0.
      *
       01 campo-string.
          02 string-a                  pic x(40) value spaces.
          02 filler                    pic x(01) value low-values.
      *
       01 campo-lixo.
         02 lixo                       pic x(1024) value spaces.
         02 filler                     pic x(01) value low-values. 
      *
       01 buffer2.
          02 filler                    pic 9(04) occurs 2000.
      *
       01 campo-rotina-ab02.
          02 rotina-ab02-codigo        pic 9(05) value 0.
          02 rotina-ab02-nivel         pic 9(01) value 0.
      *
       01 campo-rotina-cia.
          02 rotina-col-cia            pic 9(02) value 0.
          02 rotina-lin-cia            pic 9(02) value 0.
          02 rotina-borda-cia          pic x(01) value spaces.
          02 rotina-fundo-cia          pic x(01) value spaces.
          02 rotina-sombra-cia         pic x(01) value spaces.
          02 rotina-tipo-cia           pic 9(02) value 0.
          02 rotina-cia                pic x(02) value spaces.
      *
       01 campo-rotina-cid.
          02 rotina-col-cid            pic 9(02) value 0.
          02 rotina-lin-cid            pic 9(02) value 0.
          02 rotina-borda-cid          pic x(01) value spaces.
          02 rotina-fundo-cid          pic x(01) value spaces.
          02 rotina-sombra-cid         pic x(01) value spaces.
          02 rotina-tipo-cid           pic 9(02) value 0.
          02 rotina-cid                pic x(03) value spaces.
      *
       01 cab-abav.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(01) value x"0e".
          02 filler                    pic x(08) value "ABAV/SP ".
          02 filler                    pic x(01) value x"14".
          02 filler                    pic x(01) value x"0f".
          02 filler                    pic x(65) value
          "Associcao Brasileira de Agencias de Viagens de Sao Paulo - BA
      -   "LCAO".
          02 filler                    pic x(01) value x"12".
      *
       01 cab-prog.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(19) value
          "Relacao de T.K.T.'S".
          02 filler                    pic x(34) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-01.
          02 filler                    pic x(08) value spaces.
          02 filler                    pic x(03) value "CIA".
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(08) value "EMBARQUE".
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(04) value "HORA".
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(03) value "VOO".
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(03) value "PAX".
          02 filler                    pic x(19) value spaces.
          02 filler                    pic x(04) value "DEST".
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(05) value "AEROP".
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(06) value "CODIGO".
          02 filler                    pic x(14) value spaces.
      *
       01 cab-02.
          02 filler                    pic x(08) value spaces.
          02 cab-cia                   pic x(03) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-emb                   pic x(08) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-hora                  pic 99b99 value 0.
          02 filler                    pic x(02) value spaces.
          02 cab-voo                   pic 9(04) value 0.
          02 filler                    pic x(02) value spaces.
          02 cab-pax                   pic x(20) value spaces.
          02 filler                    pic x(02) value spaces.
          02 cab-destino               pic x(03) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-aerop                 pic x(03) value spaces.
          02 filler                    pic x(04) value spaces.
          02 cab-codigo                pic 9(05) value 0.
          02 filler                    pic x(16) value spaces.
      *
       01 cab-03.
          02 filler                    pic x(14) value "CODIGO......:".
          02 cab-codigo-i              pic 9(05) value 0.
          02 filler                    pic x(21) value spaces.
          02 filler                    pic x(15) value "PAX..........:".
          02 cab-pax-i                 pic x(20) value spaces.
      *
       01 cab-04.
          02 filler                    pic x(14) value "AGENCIA.....:".
          02 cab-cons-i                pic 9(05) value 0.
          02 filler                    pic x(01) value spaces.
          02 cab-dcons-i               pic x(40) value spaces.
      *
       01 cab-05.
          02 filler                    pic x(14) value "EMPRESA.....:".
          02 cab-empresa-i             pic x(20) value spaces.
          02 filler                    pic x(06) value spaces.
          02 filler                    pic x(15) value "RECEBIMENTO..:".
          02 cab-rec-i                 pic x(08) value spaces.
      *
       01 cab-06.
          02 filler                    pic x(14) value "EMBARQUE....:".
          02 cab-emb-i                 pic x(08) value spaces.
          02 filler                    pic x(18) value spaces.
          02 filler                    pic x(15) value "DESTINO......:".
          02 cab-destino-i             pic x(03) value spaces.
      *
       01 cab-07.
          02 filler                    pic x(14) value "H. EMB......:".
          02 cab-horario-e-i           pic x(05) value spaces.
          02 filler                    pic x(21) value spaces.
          02 filler                    pic x(15) value "VOO..........:".
          02 cab-voo-i                 pic 9(03) value 0.
      *
       01 cab-08.
          02 filler                    pic x(14) value "CIA.AEREA...:".
          02 cab-cia-i                 pic x(02) value spaces.
          02 filler                    pic x(01) value spaces.
          02 cab-dcia-i                pic x(20) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(15) value "AEROPORTO....:".
          02 cab-aerop-i               pic x(03) value spaces.
      *
       01 cab-09.
          02 filler                    pic x(14) value "ENTREGA.....:".
          02 cab-ent-i                 pic x(08) value spaces.
          02 filler                    pic x(18) value spaces.
          02 filler                    pic x(15) value "H.ENTREGA....:".
          02 cab-horario-i             pic x(05) value spaces.
      *
       01 cab-10.
          02 filler                    pic x(14) value "ATENDENTE...:".
          02 cab-atendente-i           pic x(10) value spaces.
      *
       01 cab-11.
          02 filler                    pic x(14) value "OBSERVACAO..:".
          02 cab-obs1-i                pic x(40) value spaces.
      *
       01 cab-12.
          02 filler                    pic x(14) value spaces.
          02 cab-obs2-i                pic x(40) value spaces.
      *
       01 cab-13.
          02 filler                    pic x(01) value spaces.
          02 cons-codigo               pic 9(06) value 0.
          02 filler                    pic x(01) value spaces.
          02 cons-razao                pic x(40) value spaces.
          02 filler                    pic x(32) value spaces.
      *
       01 cab-14.
          02 tot-desc                  pic x(25) value spaces.
          02 filler                    pic x(30) value spaces.
          02 tot-total                 pic 9(06) value 0.
          02 filler                    pic x(20) value spaces.
      *
       copy wstab04.lib.
       copy wstab05.lib.
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
          02 line 08 column 62 foreground-color 06 background-color 01
             highlight value "Relacao/Consulta".
          02 line 09 column 05 foreground-color 06 background-color 01
             highlight value "Ordenamento....:".
          02 line 10 column 05 foreground-color 06 background-color 01
             highlight value "Agencia........:".
          02 line 11 column 05 foreground-color 06 background-color 01
             highlight value "Recebimento....:           a".
          02 line 12 column 05 foreground-color 06 background-color 01
             highlight value "Destino........:".
          02 line 13 column 05 foreground-color 06 background-color 01
             highlight value "Embarque.......:           a".
          02 line 14 column 05 foreground-color 06 background-color 01
             highlight value "Entrega........:           a".
          02 line 15 column 05 foreground-color 06 background-color 01
             highlight value "Horario Emb....:".
          02 line 16 column 05 foreground-color 06 background-color 01
             highlight value "Voo............:".
          02 line 17 column 05 foreground-color 06 background-color 01
             highlight value "Cia............:".
          02 line 18 column 05 foreground-color 06 background-color 01
             highlight value "Aeroporto......:".
          02 line 19 column 05 foreground-color 06 background-color 01
             highlight value "Texto..........:".
          02 line 20 column 05 foreground-color 06 background-color 01
             highlight value "Device.........:".
      *
       01 tela-02.
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(75) from spaces.
          02 line 22 column 10 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 22 column 12 foreground-color 05 background-color 03
             value " - Estados".
      *
       01 tela-03.
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(75) from spaces.
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
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(74) from spaces.
          02 line 22 column 05 foreground-color 02 background-color 03 
             highlight value "0".
          02 line 22 column 06 foreground-color 05 background-color 03
             value "-Cod".
          02 line 22 column 12 foreground-color 02 background-color 03 
             highlight value "1".
          02 line 22 column 13 foreground-color 05 background-color 03
             value "-Cons".
          02 line 22 column 20 foreground-color 02 background-color 03 
             highlight value "2".
          02 line 22 column 21 foreground-color 05 background-color 03
             value "-Rec".
          02 line 22 column 27 foreground-color 02 background-color 03 
             highlight value "3".
          02 line 22 column 28 foreground-color 05 background-color 03
             value "-Pax".
          02 line 22 column 34 foreground-color 02 background-color 03 
             highlight value "4".
          02 line 22 column 35 foreground-color 05 background-color 03
             value "-Emp".
          02 line 22 column 41 foreground-color 02 background-color 03 
             highlight value "5".
          02 line 22 column 42 foreground-color 05 background-color 03
             value "-Dest".
          02 line 22 column 49 foreground-color 02 background-color 03 
             highlight value "6".
          02 line 22 column 50 foreground-color 05 background-color 03
             value "-Emb".
          02 line 22 column 56 foreground-color 02 background-color 03 
             highlight value "7".
          02 line 22 column 57 foreground-color 05 background-color 03
             value "-Hora".
          02 line 22 column 64 foreground-color 02 background-color 03 
             highlight value "8".
          02 line 22 column 65 foreground-color 05 background-color 03
             value "-Voo".
          02 line 22 column 70 foreground-color 02 background-color 03 
             highlight value "9".
          02 line 22 column 71 foreground-color 05 background-color 03
             value "-Cia".
      *
       01 tela-05.
          02 line 23 column 02 foreground-color 02 background-color 03
             highlight pic x(78) from spaces.
          02 line 23 column 02 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 23 column 09 foreground-color 02 background-color 03
             highlight value "C".
          02 line 23 column 10 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 23 column 23 foreground-color 02 background-color 03
             highlight value "F".
          02 line 23 column 24 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-06.
          02 line 23 column 02 foreground-color 02 background-color 03
             highlight pic x(78) from spaces.
          02 line 23 column 02 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 23 column 09 foreground-color 02 background-color 03
             highlight value "I".
          02 line 23 column 10 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-07.
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(74) from spaces.
          02 line 22 column 11 foreground-color 05 background-color 03
             value "(".
          02 line 22 column 12 foreground-color 02 background-color 03
             highlight value "I".
          02 line 22 column 13 foreground-color 05 background-color 03
             value ")mprimir   (".
          02 line 22 column 25 foreground-color 02 background-color 03
             highlight value "P".
          02 line 22 column 26 foreground-color 05 background-color 03
             value ")osicionar   (".
          02 line 22 column 40 foreground-color 02 background-color 03
             highlight value "E".
          02 line 22 column 41 foreground-color 05 background-color 03
             value ")ncerrar".
      *
       01 tela-08.
          02 line 22 column 07 foreground-color 02 background-color 03
             highlight pic x(74) from spaces.
          02 line 22 column 06 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 22 column 13 foreground-color 02 background-color 03
             highlight value "C".
          02 line 22 column 14 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 22 column 27 foreground-color 02 background-color 03
             highlight value "F".
          02 line 22 column 28 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-09.
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(74) from spaces.
          02 line 22 column 06 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 22 column 13 foreground-color 02 background-color 03
             highlight value "I".
          02 line 22 column 14 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-10.
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(74) from spaces.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 22 column 07 foreground-color 05 background-color 03
             value "-Consorciadas".
      *
       01 tela-11.
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(74) from spaces.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 22 column 07 foreground-color 05 background-color 03
             value "-Cidades".
      *
       01 tela-12.
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(74) from spaces.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 22 column 07 foreground-color 05 background-color 03
             value "-Cias. Aereas".
      *
       01 tela-13.
          02 line 23 column 02 foreground-color 02 background-color 03
             highlight pic x(78) from spaces.
          02 line 23 column 02 foreground-color 05 background-color 03
             value " Tecle <Enter>".
      *
       01 tela-14.
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(74) from spaces.
          02 line 22 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 22 column 10 foreground-color 05 background-color 03
             value "- CGH".
          02 line 22 column 18 foreground-color 02 background-color 03
             highlight value "2".
          02 line 22 column 20 foreground-color 05 background-color 03
             value "- GRU".
      *
       01 tela-15.
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(74) from spaces.
          02 line 22 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 22 column 10 foreground-color 05 background-color 03
             value "- Console".
          02 line 22 column 23 foreground-color 02 background-color 03
             highlight value "2".
          02 line 22 column 25 foreground-color 05 background-color 03
             value "- Impressora".
      *
       01 tela-16.
          02 line 22 column 04 foreground-color 02 background-color 03
             highlight pic x(74) from spaces.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 22 column 07 foreground-color 05 background-color 03
             value "-Todos".
      *
       01 tela-mensagem-cad.
          02 line 22 column 04 foreground-color 07 background-color 01
             highlight pic x(74) from mensagem.
      *
       01 tela-erro-cad.
          02 line 22 column 04 beep reverse-video pic x(74) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 22 column 04 foreground-color 01 background-color 01
             pic x(74) from spaces.
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
           move 06 to box-lin.
           move 77 to box-col-f.
           move 22 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           move spaces to box-fundo.
           move "N" to box-sombra.
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
           move ab05-pax-a to cab-pax.
           move ab05-destino to cab-destino.
           if ab05-data-emb not = 0
              move ab05-data-emb to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to cab-emb
           else
              move "ABERTO" to cab-emb
           end-if
           move ab05-horario-e to cab-hora.
           inspect cab-hora replacing all " " by ":".
           move ab05-cia to cab-cia.
           move ab05-voo to cab-voo.
           if ab05-aerop = 1
              move "CGH" to cab-aerop
           else
              move "GRU" to cab-aerop
           end-if.
           move ab05-codigo to cab-codigo.
           move ab05-cons to ab02-codigo cons-codigo.
           perform rot-le-ab02.
           if erro not = 0
              move "Consorciada nao cadastrada" to cons-razao
           else
              move ab02-razao-social-a to cons-razao
           end-if.
      *
       rot-move-i.
           move ab05-codigo to cab-codigo-i.
           move ab05-cons to cab-cons-i.
           move ab05-data-rec to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-rec-i.
           move ab05-pax-a to cab-pax-i.
           move ab05-empresa-a to cab-empresa-i.
           move ab05-destino to cab-destino-i.
           if ab05-data-emb not = 0
              move ab05-data-emb to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to cab-emb-i
           else
              move "ABERTO" to cab-emb-i
           end-if
           move ab05-horario-e to cab-hora.
           inspect cab-hora replacing all " " by ":".
           move cab-hora to cab-horario-e-i.
           move ab05-cia to cab-cia-i.
           move ab05-voo to cab-voo-i.
           if ab05-aerop = 1 
              move "CGH" to cab-aerop-i
           else
              move "GRU" to cab-aerop-i
           end-if.
           if ab05-data-ent not = 0
              move ab05-data-ent to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to cab-ent-i
              move ab05-horario to cab-hora
              inspect cab-hora replacing all " " by ":"
              move cab-hora to cab-horario-i
           else
              move spaces to cab-ent-i cab-horario-i
           end-if.
           move ab05-atendente to cab-atendente-i.
           move ab05-obs (01) to cab-obs1-i.
           move ab05-obs (02) to cab-obs2-i.
           move ab05-cons to ab02-codigo.
           perform rot-le-ab02.
           if erro not = 0
              move "Consorciada nao cadastrada" to cab-dcons-i
           else
              move ab02-razao-social-a to cab-dcons-i
           end-if.
           move 09 to wtab05-tipo.
           move ab05-cia to wtab05-sigla.
           move spaces to wtab05-resto.
           move wtab05-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              move "Cia. aerea nao cadastrada" to cab-dcia-i
           else
              move reg-tabl to reg-wtab05
              move wtab05-descricao to cab-dcia-i
           end-if.
      *
       rot-cabec.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move 8 to linha.
           add 1 to pagina
           move pagina to cab-pagina
           if pagina = 1
              write reg-imp from cab-abav
              write reg-imp from tracos-i after 1 line
              write reg-imp from cab-prog after 1 line
           else
              write reg-imp from cab-abav after page
              write reg-imp from tracos-i after 1 line   
              write reg-imp from cab-prog after 2 lines
           end-if.
           write reg-imp from tracos-i after 1 line.
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
       rot-le-ab02.
           move 0 to erro.
           read arqab02 invalid key move 1 to erro.
           if ab02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab02.
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
       rot-le-anterior.
           move 0 to erro.
           read arqab05 previous at end move 1 to erro.
           if ab05-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqab05 next at end move 1 to erro.
           if ab05-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-ab05.
           move 0 to erro.
           if ab05-stat = "F"
              open i-o arqab05
              if ab05-status not = "00"
                 move 
                 " Erro de abertura no ARQAB05A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
                 move zeros to reg-ab05
                 move high-values to ab05-controle
                 move 0 to ab05-numero
                 write reg-ab05-1
               else
                  move "A" to ab05-stat
               end-if
           end-if.
      *
       rot-close-ab05.
           if ab05-stat = "A"
              close arqab05
              move "F" to ab05-stat
           end-if.
      *
       rot-erro-leitura-ab05.
           move " Erro de leitura - ARQAB05A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
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
                display tela-06
             end-if
           end-if.
      *
       pesq-cons.
           move 1 to rotina-ab02-nivel.
           call "pgab02" using param-menu campo-rotina-ab02.
           cancel "pgab02".
      *
       rot-pesq-cid.
           move 15 to rotina-col-cid.
           move 11 to rotina-lin-cid.
           move "3" to rotina-borda-cid.
           move spaces to rotina-fundo-cid.
           move "S" to rotina-sombra-cid.
           move 8 to rotina-tipo-cid.
           call "pgtab03" using param-menu 
                                campo-rotina-cid.
          cancel "pgtab03".
      *
       rot-interrompe-console.
           call "C_Readkey".
           move return-code to campo-kbd.
           if kbd2 = 73 or 105
              display tela-05
              perform until kbd2 = 67 or 99 or 70 or 102
                      perform rot-keypress
              end-perform
             if kbd2 = 70 or 102
                move "F" to resposta
             else
                display tela-06
             end-if
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
       rot-pesq-cia.
           move 15 to rotina-col-cia.
           move 11 to rotina-lin-cia.
           move "3" to rotina-borda-cia.
           move spaces to rotina-fundo-cia.
           move "S" to rotina-sombra-cia.
           move 9 to rotina-tipo-cia.
           call "pgtab04" using param-menu 
                                campo-rotina-cia.
          cancel "pgtab04".
      *
       rot-print.
           if linha not > 23
              call "C_Writexy" using by value coluna
                                     by value linha
                                     by value tamanho
                                     by value box-cor-f
                                     by value box-cor-p
                                     by reference linha-detalhe
              add 1 to linha
           end-if.
      *
       err-codigo-n.
           move " Codigo nao cadastrado - Tecle <Enter>" to mensagem.
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
       err-cidade-n.
           move " Cidade nao cadastrada - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-cia-n.
           move " Cia. Aerea nao cadastrada - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-atendente-n.
           move " Atendente nao cadastrado - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       err-data-emb.
           move " Data embarque menor que recebimento - Tecle <Enter>"
           to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
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
      *  Sequencia para dar Accept
      *
       acc-ord.
           accept sele-ord at 0922 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cons.
           accept sele-cons at 1022 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-rec-i.
           accept sele-data-rec-i at 1122 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-rec-f.
           accept sele-data-rec-f at 1135 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-destino.
           accept sele-destino at 1222 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-emb-i.
           accept sele-data-emb-i at 1322 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-emb-f.
           accept sele-data-emb-f at 1335 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-ent-i.
           accept sele-data-ent-i at 1422 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-data-ent-f.
           accept sele-data-ent-f at 1435 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-horario-e.
           accept sele-horario-e at 1522 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
      *
       acc-voo.
           accept sele-voo at 1622 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cia.
           accept sele-cia at 1722 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-aerop.
           accept sele-aerop at 1822 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-texto.
           accept sele-texto at 1922 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-device.
           accept sele-device at 2022 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 0922 with foreground-color 15 
                   background-color 01.
      *
       dsp-cons.
           display sele-cons-disp at 1022 with
                  foreground-color 15 background-color 01.
           display sele-dcons at 1028 with
                  foreground-color 15 background-color 01.
      *
       dsp-data-rec-i.
           display sele-data-disp at 1122 with
                  foreground-color 15 background-color 01.
      *
       dsp-data-rec-f.
           display sele-data-disp at 1135 with
                  foreground-color 15 background-color 01.
      *
       dsp-destino.
           display sele-destino-disp at 1222 with
                  foreground-color 15 background-color 01.
      *
       dsp-data-emb-i.
           display sele-data-disp at 1322 with
                  foreground-color 15 background-color 01.
      *
       dsp-data-emb-f.
           display sele-data-disp at 1335 with
                  foreground-color 15 background-color 01.
      *
       dsp-data-ent-i.
           display sele-data-disp at 1422 with
                  foreground-color 15 background-color 01.
      *
       dsp-data-ent-f.
           display sele-data-disp at 1435 with
                  foreground-color 15 background-color 01.
      *
       dsp-horario-e.
           inspect sele-horario-e-aux replacing all " " by ":".
           if sele-horario-e not = 9999
              move sele-horario-e-aux to sele-horario-e-disp
           end-if.
           display sele-horario-e-disp at 1522 with
                  foreground-color 15 background-color 01.
      *
       dsp-voo.
           display sele-voo-disp at 1622 with
                  foreground-color 15 background-color 01.
      *
       dsp-cia.
           display sele-cia-disp at 1722 with
                  foreground-color 15 background-color 01.
           if sele-dcia not = spaces
              display sele-dcia at 1725 with
                     foreground-color 15 background-color 01
           end-if.
      *
       dsp-aerop.
           display sele-aerop-disp at 1822 with
                  foreground-color 15 background-color 01.
      *
       dsp-texto.
           display sele-texto at 1922 with
                  foreground-color 15 background-color 01.
      *
       dsp-device.
           display sele-device at 2022 with
                  foreground-color 15 background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 0922 with foreground-color 15 
                   background-color 01.
      *
       lmp-cons.
           display limpa at 1022 with
                  foreground-color 15 background-color 01.
      *
       lmp-data-rec-i.
           display limpa-08 at 1122 with
                  foreground-color 15 background-color 01.
      *
       lmp-data-rec-f.
           display limpa-08 at 1135 with
                  foreground-color 15 background-color 01.
      *
       lmp-destino.
           display limpa at 1222 with
                  foreground-color 15 background-color 01.
      *
       lmp-data-emb-i.
           display limpa-08 at 1322 with
                  foreground-color 15 background-color 01.
      *
       lmp-data-emb-f.
           display limpa-08 at 1335 with
                  foreground-color 15 background-color 01.
      *
       lmp-data-ent-i.
           display limpa-08 at 1422 with
                  foreground-color 15 background-color 01.
      *
       lmp-data-ent-f.
           display limpa-08 at 1435 with
                  foreground-color 15 background-color 01.
      *
       lmp-horario-e.
           display limpa-08 at 1522 with
                  foreground-color 15 background-color 01.
      *
       lmp-voo.
           display limpa-08 at 1622 with
                  foreground-color 15 background-color 01.
      *
       lmp-cia.
           display limpa at 1722 with
                  foreground-color 15 background-color 01.
      *
       lmp-aerop.
           display limpa at 1822 with
                  foreground-color 15 background-color 01.
      *
       lmp-texto.
           display limpa at 1922 with
                  foreground-color 15 background-color 01.
      *
       lmp-device.
           display limpa at 2022 with
                  foreground-color 15 background-color 01.
      *
       sec-selecao section.
      *
       lab-sele-00.
           display tela-limpa-cad.
           perform rot-open-tabl
           if erro not = 0
              go to lab-sele-fim
           end-if.
           perform rot-open-ab02.
           if erro not = 0
              go to lab-sele-fim
           end-if.
      *
       lab-sele-01.  
           display tela-04.
           move 0 to sele-ord.
           perform lmp-ord.
           perform acc-ord.
           if escape-key = 1
              go to lab-sele-fim
           end-if.
           move 0 to sele-cons.
      *
       lab-sele-02.
           display tela-10.
           move spaces to sele-dcons.
           perform lmp-cons.
           perform acc-cons.
           if escape-key = 1
              perform lmp-cons
              go to lab-sele-01
           end-if.
           if escape-key = 3
              perform pesq-cons
              move rotina-ab02-codigo to sele-cons
              go to lab-sele-02
           end-if.
           if sele-cons = 0
              move "Todas" to sele-cons-disp
              move spaces to sele-dcons
              perform dsp-cons
              display tela-limpa-cad
              go to lab-sele-03
           end-if.
           move sele-cons to ab02-codigo sele-cons-disp.
           perform rot-le-ab02.
           if erro not = 0
              perform err-codigo-n
              go to lab-sele-02
           end-if.
           move ab02-razao-social-a to sele-dcons.
           perform dsp-cons.
           display tela-limpa-cad.
      *
       lab-sele-03.
           move 0 to sele-data-rec-i
           perform lmp-data-rec-i.
           perform acc-data-rec-i.
           if escape-key = 1
              move 0 to sele-cons
              perform lmp-data-rec-i
              go to lab-sele-02
           end-if.
           if sele-data-rec-i = 0
              move "Inicial" to sele-data-disp
              perform dsp-data-rec-i
              go to lab-sele-04
           end-if.
           move sele-data-rec-i to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-sele-03
           end-if.
           move data-disp to sele-data-disp.
           move dias-corr to sele-data-rec-i
           perform dsp-data-rec-i.
      *
       lab-sele-04.
           move 0 to sele-data-rec-f
           perform lmp-data-rec-f.
           perform acc-data-rec-f.
           if escape-key = 1
              perform lmp-data-rec-f
              go to lab-sele-03
           end-if.
           if sele-data-rec-f = 0
              move 999999 to sele-data-rec-f
              move "Final" to sele-data-disp
              perform dsp-data-rec-f
              go to lab-sele-05
           end-if.
           move sele-data-rec-f to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-sele-04
           end-if.
           move data-disp to sele-data-disp.
           move dias-corr to sele-data-rec-f
           perform dsp-data-rec-f.
           move spaces to sele-destino.
      *
       lab-sele-05.
           display tela-11.
           perform lmp-destino.
           perform acc-destino.
           if escape-key = 1
              perform lmp-destino
              display tela-limpa-cad
              go to lab-sele-04
           end-if.
           if escape-key = 3
              perform rot-pesq-cid
              move rotina-cid to sele-destino
              go to lab-sele-05
           end-if.
           if sele-destino = spaces
              move "Todos" to sele-destino-disp
              perform dsp-destino
              display tela-limpa-cad
              go to lab-sele-06
           end-if.
           move sele-destino to txt.
           perform rot-texto.
           move txt to sele-destino sele-destino-disp.
           perform dsp-destino.
           move 08 to wtab04-tipo.
           move sele-destino to wtab04-sigla.
           move spaces to wtab04-resto.
           move wtab04-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-cidade-n
              go to lab-sele-05
           end-if.
           move reg-tabl to reg-wtab04.
           display tela-limpa-cad.
      *
       lab-sele-06.
           move 0 to sele-data-emb-i
           perform lmp-data-emb-i.
           perform acc-data-emb-i.
           if escape-key = 1
              perform lmp-data-emb-i
              go to lab-sele-05
           end-if.
           if sele-data-emb-i = 0
              move "Inicial" to sele-data-disp
              perform dsp-data-emb-i
              go to lab-sele-07
           end-if.
           move sele-data-emb-i to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-sele-06
           end-if.
           move data-disp to sele-data-disp.
           move dias-corr to sele-data-emb-i.
           perform dsp-data-emb-i.
      *
       lab-sele-07.
           move 0 to sele-data-emb-f
           perform lmp-data-emb-f.
           perform acc-data-emb-f.
           if escape-key = 1
              perform lmp-data-emb-f
              go to lab-sele-06
           end-if.
           if sele-data-emb-f = 0
              move 999999 to sele-data-emb-f
              move "Final" to sele-data-disp
              perform dsp-data-emb-f
              go to lab-sele-07-1
           end-if.
           move sele-data-emb-f to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-sele-07
           end-if.
           move data-disp to sele-data-disp.
           move dias-corr to sele-data-emb-f
           perform dsp-data-emb-f.
      *
       lab-sele-07-1.
           move 0 to sele-data-ent-i
           perform lmp-data-ent-i.
           perform acc-data-ent-i.
           if escape-key = 1
              perform lmp-data-ent-i
              go to lab-sele-07
           end-if.
           if sele-data-ent-i = 0
              move "Inicial" to sele-data-disp
              perform dsp-data-ent-i
              go to lab-sele-07-2
           end-if.
           move sele-data-ent-i to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-sele-07-1
           end-if.
           move data-disp to sele-data-disp.
           move dias-corr to sele-data-ent-i.
           perform dsp-data-ent-i.
      *
       lab-sele-07-2.
           move 0 to sele-data-ent-f
           perform lmp-data-ent-f.
           perform acc-data-ent-f.
           if escape-key = 1
              perform lmp-data-ent-f
              go to lab-sele-07-1
           end-if.
           if sele-data-ent-f = 0
              move 999999 to sele-data-ent-f
              move "Final" to sele-data-disp
              perform dsp-data-ent-f
              go to lab-sele-08
           end-if.
           move sele-data-ent-f to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform err-data-i
              go to lab-sele-07-2
           end-if.
           move data-disp to sele-data-disp.
           move dias-corr to sele-data-ent-f
           perform dsp-data-ent-f.
     *
       lab-sele-08.
           display tela-16.
           move 0 to sele-horario-e
           perform lmp-horario-e.
           perform acc-horario-e.
           if escape-key = 1
              perform lmp-horario-e
              display tela-limpa-cad
              go to lab-sele-07-2
           end-if.
           if escape-key = 3
              move "Todos" to sele-horario-e-disp
              move 9999 to sele-horario-e
              perform dsp-horario-e
              display tela-limpa-cad
              go to lab-sele-09
           end-if.
           move sele-horario-e to horario-r sele-horario-e-aux.
           if hora-r > 23 or min-r > 59
              go to lab-sele-08
           end-if.
           perform dsp-horario-e.
           display tela-limpa-cad.
      *
       lab-sele-09.
           move 0 to sele-voo.
           perform lmp-voo.
           perform acc-voo.
           if escape-key = 1 
              perform lmp-voo
              go to lab-sele-08
           end-if.
           if sele-voo = 0
              move "Todos" to sele-voo-disp
              perform dsp-voo
           end-if.
           move spaces to sele-cia.
      *
       lab-sele-10.
           display tela-12
           perform lmp-cia.
           perform acc-cia.
           if escape-key = 1
              perform lmp-cia
              display tela-limpa-cad
              go to lab-sele-09
           end-if.
           if escape-key = 3
              perform rot-pesq-cia
              move rotina-cia to sele-cia
              go to lab-sele-10
           end-if.
           if sele-cia = spaces 
              move "Todas" to sele-cia-disp
              move spaces to sele-dcia
              perform dsp-cia
              display tela-limpa-cad
              go to lab-sele-11
           end-if.
           move sele-cia to txt.
           perform rot-texto.
           move txt to sele-cia sele-cia-disp.
           move spaces to sele-dcia.
           perform dsp-cia.
           move 09 to wtab05-tipo.
           move sele-cia to wtab05-sigla.
           move spaces to wtab05-resto.
           move wtab05-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-cia-n
              go to lab-sele-10
           end-if.
           move reg-tabl to reg-wtab05.
           move wtab05-descricao to sele-dcia.
           perform dsp-cia.
           display tela-limpa-cad.
      *
       lab-sele-11.
           display tela-14.
           move 0 to sele-aerop.
           perform lmp-aerop.
           perform acc-aerop.
           if escape-key = 1
              perform lmp-aerop
              go to lab-sele-10
           end-if.
           if sele-aerop > 2
              go to lab-sele-11
           end-if.
           if sele-aerop = 0
              move "Todos" to sele-aerop-disp
              perform dsp-aerop
           end-if.
           display tela-limpa-cad.
      *
       lab-sele-12.
           move spaces to sele-texto.
           perform lmp-texto.
           perform acc-texto.
           if escape-key = 1
              perform lmp-texto
              go to lab-sele-11
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
       lab-sele-13.
           display tela-15.
           move 0 to sele-device.
           perform lmp-device.
           perform acc-device.
           if escape-key = 1
              perform lmp-device
              display tela-limpa-cad
              go to lab-sele-12
           end-if.
           if sele-device not = 1 and 2
              go to lab-sele-13
           end-if.
      *
       lab-sele-14.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-13
           end-if.
           if resposta = "N"
              perform lmp-ord thru lmp-device
              display tela-limpa-cad
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-14
              end-if
           end-if.
           if sele-device = 1
              perform sec-consulta
           else
              perform sec-imprime
           end-if.
           perform lmp-ord thru lmp-device.
           display tela-limpa-cad.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform rot-close-tabl.
           perform rot-close-ab05.
           exit.
      *
       sec-consulta section.
      *
       sec-cns-00.
           move 01 to box-col.
           move 03 to box-lin.
           move 78 to box-col-f.
           move 22 to box-lin-f. 
           call "C_Savescr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
           display tela-cabec.
           move spaces to box-borda.
           move 00 to box-cor-f.
           move 15 to box-cor-p.
           move spaces to box-fundo.
           move "N" to box-sombra.
           perform rot-box.
           perform rot-open-ab05.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           move 99 to linha.
           move 0 to total sub-total.
           move spaces to cons-ant.
           evaluate true
                  when sele-ord = 0
                       move low-values to ab05-chave
                       start arqab05 key is not less ab05-chave
                  when sele-ord = 1
                       move sele-cons to ab05-chave-1
                       start arqab05 key is not less ab05-chave-1
                  when sele-ord = 2
                       move sele-data-rec-i to ab05-chave-2
                       start arqab05 key is not less ab05-chave-2
                  when sele-ord = 3
                       move low-values to ab05-chave-3
                       start arqab05 key is not less ab05-chave-3
                  when sele-ord = 4
                       move low-values to ab05-chave-4
                       start arqab05 key is not less ab05-chave-4
                  when sele-ord = 5
                       move sele-destino to ab05-chave-5
                       start arqab05 key is not less ab05-chave-5
                  when sele-ord = 6
                       move sele-data-emb-i to ab05-chave-6
                       start arqab05 key is not less ab05-chave-6
                  when sele-ord = 7
                       move sele-horario-e to ab05-chave-7
                       start arqab05 key is not less ab05-chave-7
                  when sele-ord = 8
                       move sele-voo to ab05-chave-8
                       start arqab05 key is not less ab05-chave-8
                  when sele-ord = 9
                       move sele-cia to ab05-chave-9
                       start arqab05 key is not less ab05-chave-9
           end-evaluate.
           display tela-06.
      *
       lab-cns-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           if ab05-chave = high-values
              go to lab-cns-01
           end-if.
           perform rot-interrompe-console.
           if resposta = "F"
              move 27 to kbd2
              go to lab-cns-fim
           end-if.
           if sele-cons not = 0
              if ab05-cons not = sele-cons
                 go to lab-cns-01
              end-if
           end-if.
           if ab05-data-rec < sele-data-rec-i or 
              ab05-data-rec > sele-data-rec-f  
              go to lab-cns-01
           end-if.
           if sele-destino not = spaces
              if ab05-destino not = sele-destino
                 go to lab-cns-01
              end-if
           end-if.
           if ab05-data-emb < sele-data-emb-i or 
              ab05-data-emb > sele-data-emb-f  
              go to lab-cns-01
           end-if.
           if ab05-data-ent < sele-data-ent-i or 
              ab05-data-ent > sele-data-ent-f  
              go to lab-cns-01
           end-if.
           if sele-horario-e not = 9999
              if ab05-horario-e not = sele-horario-e
                 go to lab-cns-01
              end-if
           end-if.
           if sele-voo not = 0
              if ab05-voo not = sele-voo
                 go to lab-cns-01
              end-if
           end-if.
           if sele-cia not = spaces
              if ab05-cia not = sele-cia
                 go to lab-cns-01
              end-if
           end-if.
           if sele-aerop not = 0
              if ab05-aerop not = sele-aerop
                 go to lab-cns-01
              end-if
           end-if.
           if sele-texto not = spaces
              move reg-ab05 to lixo
              call "C_strupr" using by reference campo-lixo
              call "C_Strloc" using by reference campo-lixo
                                    by reference campo-string
              if return-code = 0
                 go to lab-cns-01
              end-if
           end-if.
           if linha > 17
              if linha not = 99
                 display tela-13
                 perform rot-keypress
                 if kbd2 = 27
                    go to lab-cns-fim
                 end-if
              end-if
              move 4 to linha
              perform rot-box
              display tela-06
              move cab-01 to linha-detalhe
              perform rot-print
              move tracos-c to linha-detalhe
              perform rot-print
              if sele-ord not = 1 
                 move spaces to cons-ant
              end-if
              if cons-ant not = spaces 
                 move cab-13 to linha-detalhe
                 perform rot-print
              end-if
           end-if.
           perform rot-move.
           if ab05-cons not = cons-ant 
              if cons-ant not = spaces and sele-ord = 1
                 move tracos-c to linha-detalhe
                 perform rot-print
                 move " Total da Consorciada" to tot-desc
                 move sub-total to tot-total
                 move cab-14 to linha-detalhe
                 perform rot-print
                 move tracos-c to linha-detalhe
                 perform rot-print
                 move 0 to sub-total
              end-if
              move cab-13 to linha-detalhe
              perform rot-print
              move ab05-cons to cons-ant
           end-if.
           move cab-02 to linha-detalhe.
           perform rot-print.
           add 1 to sub-total total.
           go to lab-cns-01.
      * 
       lab-cns-fim.
           if kbd2 not = 27
              if sele-ord = 1
                 move tracos-c to linha-detalhe
                 perform rot-print
                 move " Total da Consorciada" to tot-desc
                 move sub-total to tot-total
                 move cab-14 to linha-detalhe
                 perform rot-print
                 move tracos-c to linha-detalhe
                 perform rot-print
                 move " Total Geral" to tot-desc
                 move total to tot-total
                 move cab-14 to linha-detalhe
                 perform rot-print
                 move tracos-c to linha-detalhe
                 perform rot-print
              end-if
              display tela-13
              perform rot-keypress
           end-if.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
           exit.
      *
       sec-imprime section.
      *
       lab-imp-00.
           perform rot-open-ab05.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           move 99 to linha.
           move 0 to pagina sub-total total.
           evaluate true
                  when sele-ord = 0
                       move low-values to ab05-chave
                       start arqab05 key is not less ab05-chave
                  when sele-ord = 1
                       move sele-cons to ab05-chave-1
                       start arqab05 key is not less ab05-chave-1
                  when sele-ord = 2
                       move sele-data-rec-i to ab05-chave-2
                       start arqab05 key is not less ab05-chave-2
                  when sele-ord = 3
                       move low-values to ab05-chave-3
                       start arqab05 key is not less ab05-chave-3
                  when sele-ord = 4
                       move low-values to ab05-chave-4
                       start arqab05 key is not less ab05-chave-4
                  when sele-ord = 5
                       move sele-destino to ab05-chave-5
                       start arqab05 key is not less ab05-chave-5
                  when sele-ord = 6
                       move sele-data-emb-i to ab05-chave-6
                       start arqab05 key is not less ab05-chave-6
                  when sele-ord = 7
                       move sele-horario-e to ab05-chave-7
                       start arqab05 key is not less ab05-chave-7
                  when sele-ord = 8
                       move sele-voo to ab05-chave-8
                       start arqab05 key is not less ab05-chave-8
                  when sele-ord = 9
                       move sele-cia to ab05-chave-9
                       start arqab05 key is not less ab05-chave-9
           end-evaluate.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if ab05-chave = high-values
              go to lab-imp-01
           end-if.
           perform rot-interrompe-impressora.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if sele-cons not = 0
              if ab05-cons not = sele-cons
                 go to lab-imp-01
              end-if
           end-if.
           if ab05-data-rec < sele-data-rec-i or 
              ab05-data-rec > sele-data-rec-f  
              go to lab-imp-01
           end-if.
           if sele-destino not = spaces
              if ab05-destino not = sele-destino
                 go to lab-imp-01
              end-if
           end-if.
           if ab05-data-emb < sele-data-emb-i or 
              ab05-data-emb > sele-data-emb-f  
              go to lab-imp-01
           end-if.
           if ab05-data-ent < sele-data-ent-i or 
              ab05-data-ent > sele-data-ent-f  
              go to lab-imp-01
           end-if.
           if sele-horario-e not = 9999
              if ab05-horario-e not = sele-horario-e
                 go to lab-imp-01
              end-if
           end-if.
           if sele-voo not = 0
              if ab05-voo not = sele-voo
                 go to lab-imp-01
              end-if
           end-if.
           if sele-cia not = spaces
              if ab05-cia not = sele-cia
                 go to lab-imp-01
              end-if
           end-if.
           if sele-aerop not = 0
              if ab05-aerop not = sele-aerop
                 go to lab-imp-01
              end-if
           end-if.
           if sele-texto not = spaces
              move reg-ab05 to lixo
              call "C_strupr" using by reference campo-lixo
              call "C_Strloc" using by reference campo-lixo
                                    by reference campo-string
              if return-code = 0
                 go to lab-imp-01
              end-if
           end-if.
           if linha > 45
              perform rot-cabec
           end-if.
           perform rot-move-i.
           if ab05-cons not = cons-ant 
              if sub-total not = 0 and sele-ord = 1
                 move " Total da Consorciada" to tot-desc
                 move sub-total to tot-total
                 write reg-imp from cab-14 after 1 line
                 write reg-imp from tracos-i after 1 line
                 move 0 to sub-total
              end-if
           end-if.
           write reg-imp from cab-03 after 1 line
           write reg-imp from cab-04 after 1 line
           write reg-imp from cab-05 after 1 line
           write reg-imp from cab-06 after 1 line
           write reg-imp from cab-07 after 1 line
           write reg-imp from cab-08 after 1 line
           write reg-imp from cab-09 after 1 line
           write reg-imp from cab-10 after 1 line
           write reg-imp from cab-11 after 1 line
           write reg-imp from cab-12 after 1 line
           write reg-imp from tracos-i after 1 line
           add 11 to linha
           add 1 to sub-total total
           move ab05-cons to cons-ant.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           if kbd2 not = 27
              if sele-ord = 1 and sub-total not = 0 
                 move " Total da Consorciada" to tot-desc
                 move sub-total to tot-total
                 write reg-imp from cab-14 after 1 line
                 write reg-imp from tracos-i after 1 line
                 move " Total Geral" to tot-desc
                 move total to tot-total
                 write reg-imp from cab-14 after 1 line
                 write reg-imp from tracos-i after 1 line
              end-if
           end-if.
           perform rot-close-imp.
           perform rot-close-ab05.
           exit.
      *