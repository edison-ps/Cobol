      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGRC04       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao de CTAS baixadas e canceladas :                    *
      *                                                             *
      *  Data da ultima alteracao:    10/02/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgrc04.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqbx01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is bx01-chave
                  alternate record key is bx01-chave-1 with duplicates
                  alternate record key is bx01-chave-2 with duplicates
                  file status is bx01-status.
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
       copy fdbx01.lib.
      *    
       copy fdab01.lib.
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
       01 bx01-status                  pic x(02) value "00".
       01 bx01-stat                    pic x(01) value "F".
      *
       01 nome-arq-bx01.
          02 bx01-dir                  pic x(03) value "RC2".
          02 filler                    pic x(01) value "\".
          02 bx01-nome                 pic x(08) value "ARQBX01A".
          02 filler                    pic x(01) value ".".
          02 bx01-ext                  pic x(03) value "DAT".
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
       01 tabl-status                  pic x(02) value "00".
       01 tabl-stat                    pic x(01) value "F".
      *
       01 nome-arq-tabl.
          02 tabl-dir                  pic x(03) value "TBL".
          02 filler                    pic x(01) value "\".
          02 tabl-nome                 pic x(08) value "ARQTABLA".
          02 filler                    pic x(01) value ".".
          02 tabl-ext                  pic x(03) value "DAT".
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGRC04".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(48) value spaces.
       01 limpa-08                     pic x(08) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 linha                        pic 9(03) comp-5 value 0.
       01 coluna                       pic 9(04) comp-5 value 1.
       01 tamanho                      pic 9(04) comp-5 value 78.
       01 pagina                       pic 9(03) value 0.
       01 linha-detalhe                pic x(78) value spaces.
       01 tracos-c                     pic x(78) value all "Ä".
       01 tracos-i                     pic x(78) value all "-".
       01 todos                        pic x(05) value "Todos".
       01 cb-data                      pic x(08) value spaces.
       01 data-ant                     pic 9(05) value 0.
       01 sub-total                    pic 9(11)v9(02) value 0.
       01 total                        pic 9(11)v9(02) value 0.
       01 chave-aux                    pic x(06) value spaces.
      *
       01 buffer2.
          02 filler                    pic 9(04) occurs 2000.
      *
       01 campos.
          02 sele-ord                  pic 9(01) value 0.
          02 sele-condicao             pic x(01) value spaces.
          02 sele-dcondicao            pic x(10) value spaces.
          02 sele-codigo-i             pic 9(05) value 0.
          02 sele-razao-i              pic x(40) value spaces.          
          02 sele-codigo-f             pic 9(05) value 0.
          02 sele-razao-f              pic x(40) value spaces.
          02 sele-liq-i                pic 9(06) value 0.
          02 sele-liq-i-disp           pic x(08) value spaces.
          02 sele-liq-f                pic 9(06) value 0.
          02 sele-liq-f-disp           pic x(08) value spaces.
          02 sele-portador             pic 9(03) value 0.
          02 sele-dportador            pic x(40) value spaces.
          02 sele-operacao             pic 9(03) value 0.
          02 sele-doperacao            pic x(40) value spaces.
          02 sele-contas               pic 9(01) value 0.
          02 sele-device               pic 9(01) value 0.
          02 razao-aux                 pic x(40) value spaces.
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
       01 campo-rotina-cod.
          02 rotina-col-cod            pic 9(02) value 0.
          02 rotina-lin-cod            pic 9(02) value 0.
          02 rotina-borda-cod          pic x(01) value spaces.
          02 rotina-fundo-cod          pic x(01) value spaces.
          02 rotina-sombra-cod         pic x(01) value spaces.
          02 rotina-codigo-cod         pic 9(05) value 0.
          02 rotina-condicao-cod       pic x(01) value spaces.
      *
       01 cab-abav.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(01) value x"0e".
          02 filler                    pic x(08) value "ABAV/SP ".
          02 filler                    pic x(01) value x"14".
          02 filler                    pic x(01) value x"0f".
          02 filler                    pic x(65) value
          "Associcao Brasileira de Agencias de Viagens de Sao Paulo".
          02 filler                    pic x(01) value x"12".
      *
       01 cab-prog.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(39) value
          "Relacao de Contas Baixadas e Canceladas".
          02 filler                    pic x(16) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-doc-01.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(03) value "Doc".
          02 filler                    pic x(07) value spaces.
          02 filler                    pic x(04) value "Cond".
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(06) value "Codigo".
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Razao".
          02 filler                    pic x(36) value spaces.
          02 filler                    pic x(04) value "Port".
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(04) value "Oper".
          02 filler                    pic x(01) value spaces.
      *
       01 cab-doc-02.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Liquidacao".
          02 filler                    pic x(40) value spaces.
          02 filler                    pic x(05) value "Valor".
          02 filler                    pic x(31) value spaces.
      *
       01 det-doc-01.
          02 filler                    pic x(01) value spaces.
          02 doc-documento             pic x(10) value spaces.
          02 filler                    pic x(01) value spaces.
          02 doc-condicao              pic x(01) value spaces.
          02 filler                    pic x(04) value spaces.
          02 doc-codigo                pic 9(05) value 0.
          02 filler                    pic x(04) value spaces.
          02 doc-razao                 pic x(40) value spaces.
          02 filler                    pic x(02) value spaces.
          02 doc-portador              pic 9(03) value 0.
          02 filler                    pic x(03) value spaces.
          02 doc-operacao              pic 9(03) value 0.
          02 filler                    pic x(01) value spaces.
      *
       01 det-doc-02.
          02 filler                    pic x(02) value spaces.
          02 doc-liquidacao            pic x(08) value spaces.
          02 filler                    pic x(30) value spaces.
          02 doc-valor                 pic zz.zzz.zzz.zz9,99 value 0.
          02 filler                    pic x(20) value spaces.
      *
       01 det-doc-03.
          02 doc-desc-tot              pic x(20) value spaces.
          02 filler                    pic x(20) value spaces.
          02 doc-total                 pic zz.zzz.zzz.zz9,99 value 0.
          02 filler                    pic x(20) value spaces.
      *
       01 cab-cod-01.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(06) value "Codigo".
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(04) value "Cond".
          02 filler                    pic x(04) value spaces.
          02 filler                    pic x(05) value "Razao".
          02 filler                    pic x(55) value spaces.
      *
       01 cab-cod-02.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Liquidacao".
          02 filler                    pic x(04) value spaces.
          02 filler                    pic x(03) value "Doc".
          02 filler                    pic x(28) value spaces.
          02 filler                    pic x(05) value "Valor".
          02 filler                    pic x(14) value spaces.
          02 filler                    pic x(04) value "Port".
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(04) value "Oper".
          02 filler                    pic x(01) value spaces.
      *
       01 det-cod-01.
          02 filler                    pic x(01) value spaces.
          02 cod-codigo                pic 9(06) value 0.
          02 filler                    pic x(05) value spaces.
          02 cod-condicao              pic x(01) value spaces.
          02 filler                    pic x(05) value spaces.
          02 cod-razao                 pic x(40) value spaces.
          02 filler                    pic x(22) value spaces.
      *
       01 det-cod-02.
          02 filler                    pic x(02) value spaces.
          02 cod-liquidacao            pic x(08) value spaces.
          02 filler                    pic x(06) value spaces.
          02 cod-documento             pic x(10) value spaces.
          02 filler                    pic x(09) value spaces.
          02 cod-valor                 pic zz.zzz.zzz.zz9,99 value 0.
          02 filler                    pic x(15) value spaces.
          02 cod-portador              pic 9(03) value 0.
          02 filler                    pic x(04) value spaces.
          02 cod-operacao              pic 9(03) value 0.
          02 filler                    pic x(01) value spaces.
      *
       01 det-cod-03.
          02 cod-desc-tot              pic x(20) value spaces.
          02 filler                    pic x(15) value spaces.
          02 cod-total                 pic zz.zzz.zzz.zz9,99 value 0.
          02 filler                    pic x(26) value spaces.
      *
       01 cab-liq-01.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(10) value "Liquidacao".
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(06) value "Codigo".
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(04) value "Cond".
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Razao".
          02 filler                    pic x(45) value spaces.
      *
       01 cab-liq-02.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(03) value "Doc".
          02 filler                    pic x(41) value spaces.
          02 filler                    pic x(05) value "Valor".
          02 filler                    pic x(15) value spaces.
          02 filler                    pic x(04) value "Port".
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(04) value "Oper".
          02 filler                    pic x(01) value spaces.
      *
       01 det-liq-01.
          02 filler                    pic x(01) value spaces.
          02 liq-liquidacao            pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 liq-codigo                pic 9(05) value 0.
          02 filler                    pic x(04) value spaces.
          02 liq-condicao              pic x(01) value spaces.
          02 filler                    pic x(05) value spaces.
          02 liq-razao                 pic x(40) value spaces.
          02 filler                    pic x(12) value spaces.
      *
       01 det-liq-02.
          02 filler                    pic x(02) value spaces.
          02 liq-documento             pic x(10) value spaces.
          02 filler                    pic x(22) value spaces.
          02 liq-valor                 pic zz.zzz.zzz.zz9,99 value 0.
          02 filler                    pic x(16) value spaces.
          02 liq-portador              pic 9(03) value 0.
          02 filler                    pic x(04) value spaces.
          02 liq-operacao              pic 9(03) value 0.
          02 filler                    pic x(01) value spaces.
      *
       01 det-liq-03.
          02 liq-desc-tot              pic x(20) value spaces.
          02 filler                    pic x(14) value spaces.
          02 liq-total                 pic zz.zzz.zzz.zz9,99 value 0.
          02 filler                    pic x(27) value spaces.
      *
       copy workgen.lib.
       copy wstab01.lib.
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
          02 line 11 column 25 foreground-color 06 background-color 01
             highlight value "Consulta/Relacao de Contas Baixadas e Canc
      -      "eladas".
          02 line 12 column 06 foreground-color 06 background-color 01
             highlight value "Ordenamento....:".
          02 line 13 column 06 foreground-color 06 background-color 01
             highlight value "Condicao.......:".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Codigo Inicial.:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Codigo Final...:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Liquidacao.....:           a".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "Portador.......:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "Operacao.......:".
          02 line 19 column 06 foreground-color 06 background-color 01
             highlight value "Contas.........:".
          02 line 20 column 06 foreground-color 06 background-color 01
             highlight value "Device.........:".
      *
       01 tela-02.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 22 column 09 foreground-color 05 background-color 03
             value "-Baixa".
          02 line 22 column 24 foreground-color 02 background-color 03
             highlight value "2".
          02 line 22 column 25 foreground-color 05 background-color 03
             value "-Codigo".
          02 line 22 column 37 foreground-color 02 background-color 03
             highlight value "3".
          02 line 22 column 38 foreground-color 05 background-color 03
             value "-Liquidacao".
      *
       01 tela-03.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 07 foreground-color 02 background-color 03
             highlight value "1".
          02 line 22 column 08 foreground-color 05 background-color 03
             value "-Console".
          02 line 22 column 26 foreground-color 02 background-color 03
             highlight value "2".
          02 line 22 column 27 foreground-color 05 background-color 03
             value "-Impressora".
      *
       01 tela-04.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "A".
          02 line 22 column 07 foreground-color 05 background-color 03
             value " - Associados".
          02 line 22 column 25 foreground-color 02 background-color 03 
             highlight value "F".
          02 line 22 column 26 foreground-color 05 background-color 03
             value " - Afiliados".
      *
       01 tela-05.
          02 line 23 column 02 foreground-color 05 background-color 03
             pic x(78) from spaces.
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
          02 line 23 column 02 foreground-color 05 background-color 03
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 23 column 09 foreground-color 02 background-color 03
             highlight value "I".
          02 line 23 column 10 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-07.
          02 line 23 column 02 foreground-color 05 background-color 03
             pic x(78) from spaces.
          02 line 23 column 02 foreground-color 05 background-color 03
             value "Tecle <Enter>".
      *
       01 tela-08.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value " - Portadores".
      *
       01 tela-09.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value " - Operacoes".
      *
       01 tela-10.
          02 line 22 column 05 foreground-color 05 background-color 03
             pic x(68) from spaces.
          02 line 22 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 22 column 12 foreground-color 02 background-color 03
             highlight value "C".
          02 line 22 column 13 foreground-color 05 background-color 03
             value ")ontinuar   (".
          02 line 22 column 26 foreground-color 02 background-color 03
             highlight value "F".
          02 line 22 column 27 foreground-color 05 background-color 03
             value ")inalizar".
      *
       01 tela-11.
          02 line 22 column 05 foreground-color 05 background-color 03
             pic x(68) from spaces.
          02 line 22 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 22 column 12 foreground-color 02 background-color 03
             highlight value "I".
          02 line 22 column 13 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-12.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 22 column 09 foreground-color 05 background-color 03
             value "-Baixadas".
          02 line 22 column 24 foreground-color 02 background-color 03
             highlight value "2".
          02 line 22 column 25 foreground-color 05 background-color 03
             value "-Canceladas".
      *
       01 tela-13.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value " - Associados".
      *
       01 tela-mensagem-cad.
          02 line 22 column 05 foreground-color 07 background-color 01
             highlight pic x(68) from mensagem.
      *
       01 tela-erro-cad.
          02 line 22 column 05 beep reverse-video pic x(68) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 22 column 05 foreground-color 01 background-color 01
             pic x(68) from spaces.
      *
       01 tela-cabec-aux.
          02 line 24 column 01 foreground-color 07 background-color 01
             highlight pic x(80) from spaces.
          02 line 24 column 02 foreground-color 07 background-color 01
             highlight pic x(08) from cb-programa.
          02 line 24 column 12 foreground-color 07 background-color 01
             highlight pic x(08) from cb-versao.
          02 line 24 column 30 foreground-color 07 background-color 01
             value "Usr.:".
          02 line 24 column 36 foreground-color 07 background-color 01
             highlight pic x(08) from param-usr.
          02 line 24 column 50 foreground-color 07 background-color 01
             value "Imp.:".
          02 line 24 column 56 foreground-color 07 background-color 01
             highlight pic x(08) from param-impress.
          02 line 24 column 70 foreground-color 07 background-color 01
             highlight pic x(08) from cb-data.
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
           move 09 to box-lin.
           move 72 to box-col-f.
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
       rot-move-doc.
           move bx01-documento to doc-documento.
           move bx01-condicao to doc-condicao ab01-condicao.
           move bx01-codigo to doc-codigo ab01-codigo.
           perform rot-le-ab01.
           if erro not = 0
              move "Nao cadastrado" to doc-razao
           else
              move ab01-razao-social-a to doc-razao
           end-if.
           move bx01-portador to doc-portador.
           move bx01-operacao to doc-operacao.
           if bx01-liquidacao not = 0
              move bx01-liquidacao to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to doc-liquidacao
           else
              move "C/APRES." to doc-liquidacao.
           move bx01-valor-pago to doc-valor.
      *
       rot-move-cod.
           move bx01-documento to cod-documento.
           move bx01-condicao to cod-condicao ab01-condicao.
           move bx01-codigo to cod-codigo ab01-codigo.
           perform rot-le-ab01.
           if erro not = 0
              move "Nao cadastrado" to cod-razao
           else
              move ab01-razao-social-a to cod-razao
           end-if.
           move bx01-portador to cod-portador.
           move bx01-operacao to cod-operacao.
           if bx01-liquidacao not = 0
              move bx01-liquidacao to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to cod-liquidacao
           else
              move "C/APRES." to cod-liquidacao.
           move bx01-valor-pago to cod-valor.
      *
       rot-move-liq.
           move bx01-documento to liq-documento.
           move bx01-condicao to liq-condicao ab01-condicao.
           move bx01-codigo to liq-codigo ab01-codigo.
           perform rot-le-ab01.
           if erro not = 0
              move "Nao cadastrado" to liq-razao
           else
              move ab01-razao-social-a to liq-razao
           end-if.
           move bx01-portador to liq-portador.
           move bx01-operacao to liq-operacao.
           if bx01-liquidacao not = 0
              move bx01-liquidacao to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to liq-liquidacao
           else
              move "C/APRES." to liq-liquidacao.
           move bx01-valor-pago to liq-valor.
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
           write reg-imp from tracos-i after 1 line.
           evaluate true
                    when sele-ord = 1
                         write reg-imp from cab-doc-01 after 1 line
                         write reg-imp from cab-doc-02 after 1 line
                    when sele-ord = 2
                         write reg-imp from cab-cod-01 after 1 line
                         write reg-imp from cab-cod-02 after 1 line
                         move spaces to razao-aux
                    when sele-ord = 3
                         write reg-imp from cab-liq-01 after 1 line
                         write reg-imp from cab-liq-02 after 1 line
           end-evaluate.
           write reg-imp from tracos-i after 1 line.
           write reg-imp from spaces.
      *
       rot-open-bx01.
           move 0 to erro.
           if bx01-stat = "F"
              open i-o arqbx01
              if bx01-status not = "00"
                 move 
                 " Erro de abertura no ARQBX01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to bx01-stat
               end-if
           end-if.
      *
       rot-close-bx01.
           if bx01-stat = "A"
              close arqbx01
              move "F" to bx01-stat
           end-if.
      *
       rot-erro-leitura-bx01.
           move " Erro de leitura - ARQBX01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
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
       rot-pesq-tabela.
           perform rot-close-tabl.
           move 08 to rotina-col.
           move 12 to rotina-lin.
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
       rot-le-ab01.
           move 0 to erro.
           read arqab01 invalid key move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab01
           end-if.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqbx01 next at end move 1 to erro.
           if bx01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
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
       rot-erro-portador.
           move " Portador nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-erro-operacao.
           move " Operacao nao cadastrada - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-erro-codigo.
           move " Codigo nao cadastrado - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-codigo-f-m.
           move " Codigo final menor que codigo inicial - Tecle <Enter>"
                to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-data-i.
           move " Data invalida - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       rot-liq-m.
           move " Liquidacao final menor que liquidacao inicial" 
           to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
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
              display tela-10
              perform until kbd2 = 67 or 99 or 70 or 102
                      perform rot-keypress
              end-perform
             if kbd2 = 70 or 102
                move "F" to resposta
             else
                display tela-11
             end-if
           end-if.
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
       rot-pesq-associado.
           perform rot-close-ab01.
           move 08 to rotina-col-cod.
           move 11 to rotina-lin-cod.
           move "3" to rotina-borda-cod.
           move spaces to rotina-fundo-cod.
           move "S" to rotina-sombra-cod.
           move sele-condicao to rotina-condicao-cod.
           call "rotab01" using param-menu campo-rotina-cod.
           cancel "rotab01".
           perform rot-open-ab01.
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
           accept resposta at 1930 with auto foreground-color 01
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
           accept sele-ord at 1223 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-condicao.
           accept sele-condicao at 1323 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-codigo-i.
           accept sele-codigo-i at 1423 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-codigo-f.
           accept sele-codigo-f at 1523 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-liq-i.
           accept sele-liq-i at 1623 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-liq-f.
           accept sele-liq-f at 1636 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-portador.
           accept sele-portador at 1723 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-operacao.
           accept sele-operacao at 1823 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-contas.
           accept sele-contas at 1923 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-device.
           accept sele-device at 2023 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 1223 with foreground-color 15 
                   background-color 01.
      *
       dsp-condicao.
           display sele-dcondicao at 1323 with foreground-color 15 
                   background-color 01.
      *
       dsp-codigo-i.
           display sele-codigo-i at 1423 with foreground-color 15 
                   background-color 01.
           display sele-razao-i at 1430 with foreground-color 15 
                   background-color 01.
      *
       dsp-codigo-f.
           display sele-codigo-f at 1523 with foreground-color 15 
                   background-color 01.
           display sele-razao-f at 1530 with foreground-color 15 
                   background-color 01.
      *
       dsp-liq-i.
           display sele-liq-i-disp at 1623 with foreground-color 15
                   background-color 01.
      *
       dsp-liq-f.
           display sele-liq-f-disp at 1636 with foreground-color 15
                   background-color 01.
      *
       dsp-portador.
           display sele-portador at 1723 with foreground-color 15 
                   background-color 01.
           display sele-dportador at 1727 with foreground-color 15 
                   background-color 01.
      *
       dsp-operacao.
           display sele-operacao at 1823 with foreground-color 15 
                   background-color 01.
           display sele-doperacao at 1827 with foreground-color 15 
                   background-color 01.
      *
       dsp-contas.
           display sele-contas at 1923 with foreground-color 15 
                   background-color 01.
      *
       dsp-device.
           display sele-device at 2023 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1223 with foreground-color 15 
                   background-color 01.
      *
       lmp-condicao.
           display limpa at 1323 with foreground-color 15 
                   background-color 01.
      *
       lmp-codigo-i.
           display limpa at 1423 with foreground-color 15 
                   background-color 01.
      *
       lmp-codigo-f.
           display limpa at 1523 with foreground-color 15 
                   background-color 01.
      *
       lmp-liq-i.
           display limpa-08 at 1623 with foreground-color 15 
                   background-color 01.
      *
       lmp-liq-f.
           display limpa-08 at 1636 with foreground-color 15 
                   background-color 01.
      *
       lmp-portador.
           display limpa at 1723 with foreground-color 15 
                   background-color 01.
      *
       lmp-operacao.
           display limpa at 1823 with foreground-color 15 
                   background-color 01.
      *
       lmp-contas.
           display limpa at 1923 with foreground-color 15 
                   background-color 01.
      *
       lmp-device.
           display limpa at 2023 with foreground-color 15 
                   background-color 01.
      *
       sec-selecao section.
      *
       lab-sele-00.
           display tela-limpa-cad.
           perform rot-open-ab01.
           if erro not = 0
              go to lab-sele-fim
           end-if.
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
           if sele-ord not = 1 and 2 and 3
              go to lab-sele-01
           end-if.
           display tela-limpa-cad.
      *
       lab-sele-02.
           display tela-04.
           move spaces to sele-condicao.
           perform lmp-condicao.
           perform acc-condicao.
           if escape-key = 1 
              perform lmp-condicao
              display tela-limpa-cad
              go to lab-sele-01
           end-if.
           move sele-condicao to txt.
           perform rot-texto.
           if txt not = "A" and "F" and space
              go to lab-sele-02
           end-if.
           if txt = spaces
              move txt to sele-condicao
              move "Todos" to sele-dcondicao 
           else
              move txt to sele-condicao sele-dcondicao
           end-if.
           perform dsp-condicao.
           display tela-limpa-cad.
           move 0 to rotina-codigo-cod.
      *
       lab-sele-03.
           display tela-13.
           move rotina-codigo-cod to sele-codigo-i.
           move spaces to sele-razao-i.
           perform lmp-codigo-i.
           perform acc-codigo-i.
           if escape-key = 1
              perform lmp-codigo-i
              go to lab-sele-02
           end-if.
           if escape-key = 3
              perform rot-pesq-associado
              go to lab-sele-03
           end-if.
           if sele-codigo-i = 0
              go to lab-sele-04
           end-if.
           if sele-condicao not = spaces
              move sele-codigo-i to ab01-codigo
              move sele-condicao to ab01-condicao
              perform rot-le-ab01
              if erro not = 0
                 perform rot-erro-codigo
                 go to lab-sele-03
              end-if
              move ab01-razao-social-a to sele-razao-i
           end-if.
           perform dsp-codigo-i.
           move 0 to rotina-codigo-cod.
      *
       lab-sele-04.
           display tela-13.
           move rotina-codigo-cod to sele-codigo-f.
           move spaces to sele-razao-f.
           perform lmp-codigo-f.
           perform acc-codigo-f.
           if escape-key = 1
              move 0 to rotina-codigo-cod
              perform lmp-codigo-f
              go to lab-sele-03
           end-if.
           if escape-key = 3
              perform rot-pesq-associado
              go to lab-sele-04
           end-if.
           if sele-codigo-f = 0
              move 99999 to sele-codigo-f
              perform dsp-codigo-f
              display tela-limpa-cad
              go to lab-sele-05
           end-if.
           if sele-codigo-f < sele-codigo-i
              perform rot-codigo-f-m
              go to lab-sele-04
           end-if.
           if sele-condicao not = spaces
              move sele-codigo-f to ab01-codigo
              move sele-condicao to ab01-condicao
              perform rot-le-ab01
              if erro not = 0
                 perform rot-erro-codigo
                 go to lab-sele-04
              end-if
              move ab01-razao-social-a to sele-razao-f
           end-if.
           perform dsp-codigo-f.
           display tela-limpa-cad.
      *
       lab-sele-05.
           move 0 to sele-liq-i.
           perform lmp-liq-i.
           perform acc-liq-i.
           if escape-key = 1
              move 0 to rotina-codigo-cod
              perform lmp-liq-i
              go to lab-sele-04
           end-if.
           if sele-liq-i = 0
              move "Inicial" to sele-liq-i-disp
              perform dsp-liq-i
              go to lab-sele-06
           end-if.
           move sele-liq-i to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-sele-05
           end-if.
           move data-disp to sele-liq-i-disp.
           move dias-corr to sele-liq-i.
           perform dsp-liq-i.
      *
       lab-sele-06.
           move 0 to sele-liq-f.
           perform lmp-liq-f.
           perform acc-liq-f.
           if escape-key = 1
              perform lmp-liq-f
              go to lab-sele-05
           end-if.
           if sele-liq-f = 0
              move 99999 to sele-liq-f
              move "Final" to sele-liq-f-disp
              perform dsp-liq-f
              go to lab-sele-07
           end-if.
           move sele-liq-f to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-sele-06
           end-if.
           move data-disp to sele-liq-f-disp.
           move dias-corr to sele-liq-f.
           if sele-liq-f < sele-liq-i
              perform rot-liq-m
              go to lab-sele-06
           end-if.
           perform dsp-liq-f.
           move 0 to rotina-codigo.
      *
       lab-sele-07.
           move rotina-codigo to sele-portador.
           display tela-08.
           perform lmp-portador.
           perform acc-portador.
           if escape-key = 1
              display tela-limpa-cad
              perform lmp-portador
              go to lab-sele-06
           end-if.
           if escape-key = 3
              move 4 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-sele-07
           end-if.
           if sele-portador = 0
              go to lab-sele-08
           end-if.
           move 04 to wtab01-tipo.
           move sele-portador to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-portador
              go to lab-sele-07
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to sele-dportador.
           perform dsp-portador.
           display tela-limpa-cad.
           move 0 to rotina-codigo.
      *
       lab-sele-08.
           move rotina-codigo to sele-operacao.
           display tela-09.
           perform lmp-operacao.
           perform acc-operacao.
           if escape-key = 1
              move 0 to rotina-codigo
              perform lmp-operacao
              go to lab-sele-07
           end-if.
           if escape-key = 3
              move 5 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-sele-08
           end-if.
           if sele-operacao = 0
              go to lab-sele-09
           end-if.
           move 05 to wtab01-tipo.
           move sele-operacao to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-operacao
              go to lab-sele-08
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to sele-doperacao.
           perform dsp-operacao.
           display tela-limpa-cad.
      *
       lab-sele-09.
           display tela-12.
           move 0 to sele-contas.
           perform lmp-contas.
           perform acc-contas.
           if escape-key = 1
              perform lmp-contas
              go to lab-sele-08
           end-if.
           if sele-contas not = 0 and 1 and 2
              go to lab-sele-09
           end-if.
      *
       lab-sele-10.
           display tela-03.
           move 0 to sele-device.
           perform lmp-device.
           perform acc-device.
           if escape-key = 1
              perform lmp-device
              go to lab-sele-09
           end-if.
           if sele-device not = 1 and 2
              go to lab-sele-10
           end-if.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-sele-11.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-10
           end-if.
           if resposta = "N"
              perform lmp-condicao thru lmp-device
              display tela-limpa-cad
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-11
              end-if
           end-if.
           display tela-limpa-cad.
           if sele-device = 1
              perform sec-consulta
           else
              perform sec-impressao
           end-if.
           perform lmp-condicao thru lmp-device.
           display tela-limpa-cad.
           display tela-limpa.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform rot-close-tabl.
           perform rot-close-ab01.
           exit.
      *
       sec-consulta section.
       sec-cns-00.
           move 01 to box-col.
           move 03 to box-lin.
           move 78 to box-col-f.
           move 23 to box-lin-f. 
           call "C_Savescr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cb-data.
           display tela-cabec-aux.
           move spaces to box-borda.
           move 00 to box-cor-f.
           move 15 to box-cor-p.
           move 22 to box-lin-f. 
           move spaces to box-fundo.
           move "N" to box-sombra.
           perform rot-box.
           perform rot-open-bx01.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           move 99 to linha.
           move 0 to sub-total total.
           evaluate true
                    when sele-ord = 1
                         move low-values to bx01-chave
                         start arqbx01 key is not less bx01-chave
                    when sele-ord = 2
                         move low-values to bx01-chave-1
                         move sele-codigo-i to bx01-codigo
                         move sele-condicao to bx01-condicao
                         start arqbx01 key is not less bx01-chave-1
                    when sele-ord = 3
                         move low-values to bx01-chave-2
                         move sele-liq-i to bx01-liquidacao
                         start arqbx01 key is not less bx01-chave-2
           end-evaluate.
      *
       lab-cns-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-cns-fim
           end-if.
           if bx01-chave = high-values
              go to lab-cns-01
           end-if.
           perform rot-interrompe-console.
           if resposta = "F"
              move 27 to kbd2
              go to lab-cns-fim
           end-if.
           if bx01-codigo < sele-codigo-i or bx01-codigo > sele-codigo-f
              go to lab-cns-01
           end-if.
           if bx01-liquidacao < sele-liq-i or 
              bx01-liquidacao > sele-liq-f
              go to lab-cns-01
           end-if.
           if sele-condicao not = spaces
              if bx01-condicao not = sele-condicao
                 go to lab-cns-01
              end-if
            end-if.
           if sele-portador not = 0
              if bx01-portador not = sele-portador
                 go to lab-cns-01
              end-if
           end-if.
           if sele-operacao not = 0
              if bx01-operacao not = sele-operacao
                 go to lab-cns-01
              end-if
           end-if.
           if sele-contas = 1
              if bx01-valor-pago = 0
                 go to lab-cns-01
              end-if
           else
              if sele-contas = 2
                 if bx01-valor-pago not = 0
                    go to lab-cns-01
                 end-if
              end-if
           end-if.
           evaluate true
                    when sele-ord = 2
                         if bx01-chave-1 not = chave-aux and
                            sub-total not = 0
                            move tracos-c to linha-detalhe
                            perform rot-print
                            move " Total do Associado" to cod-desc-tot
                            move sub-total to cod-total
                            move det-cod-03 to linha-detalhe
                            perform rot-print
                            move tracos-c to linha-detalhe
                            perform rot-print
                            move 0 to sub-total
                         end-if
                    when sele-ord = 3
                         if bx01-liquidacao not = data-ant and 
                            sub-total not = 0
                            move tracos-c to linha-detalhe
                            perform rot-print
                            move " Total da Data" to liq-desc-tot
                            move sub-total to liq-total
                            move det-liq-03 to linha-detalhe
                            perform rot-print
                            move tracos-c to linha-detalhe
                            perform rot-print
                            move 0 to sub-total
                         end-if
           end-evaluate.
           if linha > 17
              if linha not = 99
                 display tela-07
                 perform rot-keypress
                 if kbd2 = 27
                    go to lab-cns-fim
                 end-if
              end-if
              move 4 to linha
              perform rot-box
              display tela-06
              evaluate true
                       when sele-ord = 1
                            move cab-doc-01 to linha-detalhe
                            perform rot-print
                            move cab-doc-02 to linha-detalhe
                            perform rot-print
                            move tracos-c to linha-detalhe
                            perform rot-print
                       when sele-ord = 2
                            move cab-cod-01 to linha-detalhe
                            perform rot-print
                            move cab-cod-02 to linha-detalhe
                            perform rot-print
                            move tracos-c to linha-detalhe
                            perform rot-print
                            move spaces to razao-aux
                       when sele-ord = 3
                            move cab-liq-01 to linha-detalhe
                            perform rot-print
                            move cab-liq-02 to linha-detalhe
                            perform rot-print
                            move tracos-c to linha-detalhe
                            perform rot-print
                            move 0 to data-ant
              end-evaluate
           end-if.
           evaluate true
                    when sele-ord = 1
                         perform rot-move-doc
                         move det-doc-01 to linha-detalhe
                         perform rot-print
                         move det-doc-02 to linha-detalhe
                         perform rot-print
                    when sele-ord = 2
                         perform rot-move-cod
                         if ab01-razao-social-a not = razao-aux
                            move det-cod-01 to linha-detalhe
                            perform rot-print
                            move cod-razao to razao-aux
                            move bx01-chave-1 to chave-aux
                         end-if
                         move det-cod-02 to linha-detalhe
                         perform rot-print
                    when sele-ord = 3
                         perform rot-move-liq
                         if bx01-liquidacao = data-ant 
                            move spaces to liq-liquidacao
                         end-if
                         move det-liq-01 to linha-detalhe
                         perform rot-print
                         move det-liq-02 to linha-detalhe
                         perform rot-print
                         move bx01-liquidacao to data-ant
           end-evaluate.
           if sele-contas not = 2
              add bx01-valor to sub-total total
           end-if.
           go to lab-cns-01.
      * 
       lab-cns-fim.
           if kbd2 not = 27
              evaluate true
                       when sele-ord = 1
                            if total not = 0
                               move tracos-c to linha-detalhe
                               perform rot-print
                               move " Total Geral" to doc-desc-tot
                               move total to doc-total
                               move det-doc-03 to linha-detalhe
                               perform rot-print
                               move tracos-c to linha-detalhe
                               perform rot-print
                            end-if
                       when sele-ord = 2
                            if total not = 0
                               move tracos-c to linha-detalhe
                               perform rot-print
                              move " Total do Associado" to cod-desc-tot
                               move sub-total to cod-total
                               move det-cod-03 to linha-detalhe
                               perform rot-print
                               move " Total Geral" to cod-desc-tot
                               move total to cod-total
                               move det-cod-03 to linha-detalhe
                               perform rot-print
                               move tracos-c to linha-detalhe
                               perform rot-print
                            end-if
                       when sele-ord = 3
                            if total not = 0
                               move tracos-c to linha-detalhe
                               perform rot-print
                               move " Total da Data" to liq-desc-tot
                               move sub-total to liq-total
                               move det-liq-03 to linha-detalhe
                               perform rot-print
                               move " Total Geral" to liq-desc-tot
                               move total to liq-total
                               move det-liq-03 to linha-detalhe
                               perform rot-print
                               move tracos-c to linha-detalhe
                               perform rot-print
                            end-if
              end-evaluate
              display tela-07
              perform rot-keypress
           end-if.
           perform rot-close-bx01.
           move 23 to box-lin-f. 
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
           exit.
      *
       sec-impressao section.
       lab-imp-00.
           perform rot-open-bx01.
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
                    when sele-ord = 1
                         move low-values to bx01-chave
                         start arqbx01 key is not less bx01-chave
                    when sele-ord = 2
                         move low-values to bx01-chave-1
                         move sele-codigo-i to bx01-codigo
                         move sele-condicao to bx01-condicao
                         start arqbx01 key is not less bx01-chave-1
                    when sele-ord = 3
                         move low-values to bx01-chave-2
                         move sele-liq-i to bx01-liquidacao
                         start arqbx01 key is not less bx01-chave-2
           end-evaluate.
           display tela-11.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if bx01-chave = high-values
              go to lab-imp-01
           end-if.
           perform rot-interrompe-impressora.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if bx01-codigo < sele-codigo-i or bx01-codigo > sele-codigo-f
              go to lab-imp-01
           end-if.
           if bx01-liquidacao < sele-liq-i or 
              bx01-liquidacao > sele-liq-f
              go to lab-imp-01
           end-if.
           if sele-condicao not = spaces
              if bx01-condicao not = sele-condicao
                 go to lab-imp-01
              end-if
            end-if.
           if sele-portador not = 0
              if bx01-portador not = sele-portador
                 go to lab-imp-01
              end-if
           end-if.
           if sele-operacao not = 0
              if bx01-operacao not = sele-operacao
                 go to lab-imp-01
              end-if
           end-if.
           if sele-contas = 1
              if bx01-valor-pago = 0
                 go to lab-imp-01
              end-if
           else
              if sele-contas = 2
                 if bx01-valor-pago not = 0
                    go to lab-imp-01
                 end-if
              end-if
           end-if.
           evaluate true
                    when sele-ord = 2
                         if bx01-chave-1 not = chave-aux and
                            sub-total not = 0
                            write reg-imp from tracos-i after 1 line
                            move " Total do Associado" to cod-desc-tot
                            move sub-total to cod-total
                            write reg-imp from det-cod-03 after 1 line
                            write reg-imp from tracos-i after 1 line
                            add 3 to linha
                            move 0 to sub-total
                         end-if
                    when sele-ord = 3
                         if bx01-liquidacao not = data-ant and 
                            sub-total not = 0
                            write reg-imp from tracos-i after 1 line
                            move " Total da Data" to liq-desc-tot
                            move sub-total to liq-total
                            write reg-imp from det-liq-03 after 1 line
                            write reg-imp from tracos-i after 1 line
                            add 3 to linha
                            move 0 to sub-total
                         end-if
           end-evaluate.
           if linha > 50
              perform rot-cabec
           end-if.
           evaluate true
                    when sele-ord = 1
                         perform rot-move-doc
                         write reg-imp from det-doc-01 after 1 line
                         write reg-imp from det-doc-02 after 1 line
                         add 2 to linha
                    when sele-ord = 2
                         perform rot-move-cod
                         if ab01-razao-social-a not = razao-aux
                            write reg-imp from det-cod-01 after 1 line
                            move cod-razao to razao-aux
                            move bx01-chave-1 to chave-aux
                            add 1 to linha
                         end-if
                         write reg-imp from det-cod-02 after 1 line
                         add 1 to linha
                    when sele-ord = 3
                         perform rot-move-liq
                         if bx01-liquidacao = data-ant 
                            move spaces to liq-liquidacao
                         end-if
                         write reg-imp from det-liq-01 after 1 line
                         write reg-imp from det-liq-02 after 1 line
                         add 2 to linha
                         move bx01-liquidacao to data-ant
           end-evaluate.
           if sele-contas not = 2
              add bx01-valor to sub-total total
           end-if.
           go to lab-imp-01.
      * 
       lab-imp-fim.
           if kbd2 not = 27
              evaluate true
                       when sele-ord = 1
                            if total not = 0
                               write reg-imp from tracos-i after 1 line
                               move " Total Geral" to doc-desc-tot
                               move total to doc-total
                              write reg-imp from det-doc-03 after 1 line
                               write reg-imp from tracos-i after 1 line
                            end-if
                       when sele-ord = 2
                            if total not = 0
                               write reg-imp from tracos-i after 1 line
                              move " Total do Associado" to cod-desc-tot
                               move sub-total to cod-total
                              write reg-imp from det-cod-03 after 1 line
                               move " Total Geral" to cod-desc-tot
                               move total to cod-total
                              write reg-imp from det-cod-03 after 1 line
                               write reg-imp from tracos-i after 1 line
                            end-if
                       when sele-ord = 3
                            if total not = 0
                               write reg-imp from tracos-i after 1 line
                               move " Total da Data" to liq-desc-tot
                               move sub-total to liq-total
                             write reg-imp from det-liq-03 after 1 line
                               move " Total Geral" to liq-desc-tot
                               move total to liq-total
                             write reg-imp from det-liq-03 after 1 line
                               write reg-imp from tracos-i after 1 line
                            end-if
              end-evaluate
           end-if.
           if pagina not = 0
              write reg-imp from spaces after page
           end-if.
           perform rot-close-imp.
           perform rot-close-bx01.
           exit.
      *