      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGAB06       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Relacao de Associados :                                    *
      *                                                             *
      *  Data da ultima alteracao:    25/04/94     v1.00            *
      *                               02/02/96     v1.01            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgab06.
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
           select arqimp assign to disk
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
           select arqsort assign to disk.
      *
       data division.
       file section.
      *    
       copy fdab01.lib.
      *
       copy fdab04.lib.
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
          02 sort-condicao             pic x(01).
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
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAB06".
          02 cb-versao                 pic x(06) value "v1.01 ".
      *
       01 limpa                        pic x(48) value spaces.
       01 limpa-10                     pic x(10) value spaces.
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
          02 sele-condicao             pic x(01) value spaces.
          02 sele-condicao-disp        pic x(05) value spaces.
          02 sele-situacao             pic 9(01) value 0.
          02 sele-situacao-disp        pic x(05) value spaces.
          02 sele-cep-i                pic 9(08) value 0.
          02 sele-cep-f                pic 9(08) value 0.
          02 sele-cep-i-aux            pic 99999b999.
          02 sele-cep-f-aux            pic 99999b999.
          02 sele-cep-i-disp           pic x(09) value spaces.
          02 sele-cep-f-disp           pic x(09) value spaces.
          02 sele-localidade           pic 9(01) value 0.
          02 sele-localidade-disp      pic x(05) value spaces.
          02 sele-categoria            pic 9(03) value 0.
          02 sele-dcategoria           pic x(40) value spaces.
          02 sele-ano                  pic 9(02) value 0.
          02 sele-ano-disp             pic x(05) value spaces.
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
       01 cab-abav.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(01) value x"0e".
          02 filler                    pic x(08) value "ABAV/SP ".
          02 filler                    pic x(01) value x"14".
          02 filler                    pic x(01) value x"0f".
          02 filler                    pic x(56) value
          "Associcao Brasileira de Agencias de Viagens de Sao Paulo".
          02 filler                    pic x(01) value x"12".
      *
       01 cab-prog.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(31) value
          "Relacao de Associados/Afiliados".
          02 filler                    pic x(23) value spaces.
          02 cab-data                  pic x(08) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(05) value "Pag. ".
          02 cab-pagina                pic 9(04) value 0.
      *
       01 cab-01.
          02 filler                    pic x(11) value "Empresa..:".
          02 cab-razao-social          pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(11) value "Codigo...:".
          02 cab-codigo                pic 9(05) value 0 
             blank when zero.
      *
       01 cab-02.
          02 filler                    pic x(11) value "Titular..:".
          02 cab-Titular               pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(11) value "Condicao.:".
          02 cab-condicao              pic x(01) value spaces.
      *
       01 cab-03.
          02 filler                    pic x(11) value "Endereco.:".
          02 cab-endereco              pic x(40) value spaces.
          02 filler                    pic x(01) value spaces.
          02 filler                    pic x(06) value "U.F.:".
          02 cab-uf                    pic x(02) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(08) value "C.E.P.:".
          02 cab-cep                   pic 9(05)b9(03) value 0
             blank when zero.
      *
       01 cab-04.
          02 filler                    pic x(11) value "Cidade...:".
          02 cab-cidade                pic x(15) value spaces.
          02 filler                    pic x(26) value spaces.
          02 filler                    pic x(11) value "D.D.D....:".
          02 cab-ddd                   pic 9(04) value 0
             blank when zero.
      *
       01 cab-05.
          02 filler                    pic x(11) value "Fone.....:".
          02 cab-telefone-1            pic x(08) value spaces.
          02 filler                    pic x(08) value spaces.
          02 cab-telefone-2            pic x(08) value spaces.
          02 filler                    pic x(17) value spaces.
          02 filler                    pic x(11) value "Telex....:".
          02 cab-telex                 pic x(09) value spaces.
      *
       01 cab-06.
          02 filler                    pic x(11) value "Fax......:".
          02 cab-fax                   pic x(08) value spaces.
          02 filler                    pic x(08) value spaces.
          02 filler                    pic x(11) value "Ramal....:".
          02 cab-ramal-1               pic 9(03) value 0
             blank when zero.
          02 filler                    pic x(02) value spaces.
          02 cab-ramal-2               pic 9(03) value 0
             blank when zero.      
          02 filler                    pic x(06) value spaces.
      *
       01 cab-07.
          02 filler                    pic x(10) value "Sindicato:".
          02 filler                    pic x(01) value spaces.
          02 cab-sindicato             pic 9(05) value 0
             blank when zero.
          02 filler                    pic x(11) value spaces.
          02 filler                    pic x(10) value "SNEA.....:".
          02 filler                    pic x(01) value spaces.
          02 cab-snea                  pic 9(05) value 0
             blank when zero.
          02 filler                    pic x(09) value spaces.
          02 filler                    pic x(10) value "Embratur.:".
          02 filler                    pic x(01) value spaces.
          02 cab-embratur              pic x(12) value spaces.
      *
       01 cab-08.
          02 filler                    pic x(10) value "IATA.....:".
          02 filler                    pic x(01) value spaces.
          02 cab-iata                  pic x(12) value spaces.
          02 filler                    pic x(04) value spaces.
          02 filler                    pic x(10) value "COTAL....:".
          02 filler                    pic x(01) value spaces.
          02 cab-cotal                 pic x(12) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Categoria:".
          02 filler                    pic x(01) value spaces.
          02 cab-categoria             pic 9(03) value 0.
      *
       01 cab-09.
          02 filler                    pic x(10) value "Situacao.:".
          02 filler                    pic x(01) value spaces.
          02 cab-situacao              pic 9(01) value 0.
          02 filler                    pic x(15) value spaces.
          02 filler                    pic x(10) value "C.G.C....:".
          02 filler                    pic x(01) value spaces.
          02 cab-cgc                   pic 99.999.999/9999b99
             blank when zero.
      *
       01 cab-10.
          02 filler                    pic x(10) value "Alt.Contr:".
          02 filler                    pic x(01) value spaces.
          02 cab-alteracao             pic x(08) value spaces.
          02 filler                    pic x(08) value spaces.
          02 filler                    pic x(10) value "Adesao...:".
          02 filler                    pic x(01) value spaces.
          02 cab-adesao                pic x(08) value spaces.
          02 filler                    pic x(06) value spaces.
          02 filler                    pic x(10) value "Cancel...:".
          02 filler                    pic x(01) value spaces.
          02 cab-cancelamento          pic x(08) value spaces.
      *
       01 cab-11.
          02 filler                    pic x(17) value 
          "T i t u l a r e s".
      *
       01 cab-12.
          02 filler                    pic x(10) value "Nome.....:".
          02 filler                    pic x(01) value spaces.
          02 cab-nome-t                pic x(40) value spaces.
      *
       01 cab-13.
          02 filler                    pic x(10) value "Endereco.:".
          02 filler                    pic x(01) value spaces.
          02 cab-endereco-t            pic x(40) value spaces.
      *
       01 cab-14.
          02 filler                    pic x(10) value "Cidade...:".
          02 filler                    pic x(01) value spaces.
          02 cab-cidade-t              pic x(20) value spaces.
          02 filler                    pic x(21) value spaces.
          02 filler                    pic x(10) value "U.F......:".
          02 filler                    pic x(01) value spaces.
          02 cab-uf-t                  pic x(02) value spaces.
      *
       01 cab-15.
          02 filler                    pic x(10) value "C.E.P....:".
          02 filler                    pic x(01) value spaces.
          02 cab-cep-t                 pic 99999b999 value 0.
          02 filler                    pic x(07) value spaces.
          02 filler                    pic x(10) value "D.D.D....:".
          02 filler                    pic x(01) value spaces.
          02 cab-ddd-t                 pic 9(04) value 0.
          02 filler                    pic x(10) value spaces.
          02 filler                    pic x(10) value "Telefone.:".
          02 filler                    pic x(01) value spaces.
          02 cab-telefone-t            pic x(08) value spaces.
      *
       01 cab-16.
          02 filler                    pic x(10) value "Dt.Nasc..:".
          02 filler                    pic x(01) value spaces.
          02 cab-dt-nasc-t             pic x(08) value spaces.
          02 filler                    pic x(08) value spaces.
          02 filler                    pic x(10) value "Cargo....:".
          02 filler                    pic x(01) value spaces.
          02 cab-cargo-t               pic x(10) value spaces.
          02 filler                    pic x(04) value spaces.
          02 filler                    pic x(10) value "Nascional:".
          02 filler                    pic x(01) value spaces.
          02 cab-nascionalidade-t      pic x(10) value spaces.
      *
       01 cab-17.
          02 filler                    pic x(10) value "R.G......:".
          02 filler                    pic x(01) value spaces.
          02 cab-rg-t                  pic 999.999.999 
             blank when zero.
          02 filler                    pic x(05) value spaces.
          02 filler                    pic x(10) value "Emissor..:".
          02 filler                    pic x(01) value spaces.
          02 cab-emissor-t             pic x(10) value spaces.
          02 filler                    pic x(04) value spaces.
          02 filler                    pic x(10) value "C.P.F....:".
          02 filler                    pic x(01) value spaces.
          02 cab-cpf-t                 pic 999.999.999b99 
             blank when zero.
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
          02 line 12 column 64 foreground-color 06 background-color 01
             highlight value "Relacao".
          02 line 13 column 06 foreground-color 06 background-color 01
             highlight value "Ordenamento...:".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Tipo..........:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Condicao......:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Situacao......:".
          02 line 16 column 35 foreground-color 06 background-color 01
             highlight value "Ano...........:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "C.e.p. Inicial:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "C.e.p. Final..:".
          02 line 19 column 06 foreground-color 06 background-color 01
             highlight value "Localidade....:".
          02 line 20 column 06 foreground-color 06 background-color 01
             highlight value "Categoria.....:".
      *
       01 tela-02.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 22 column 07 foreground-color 02 background-color 03
             highlight value "1".
          02 line 22 column 08 foreground-color 05 background-color 03
             value "-Codigo".
          02 line 22 column 21 foreground-color 02 background-color 03
             highlight value "2".
          02 line 22 column 22 foreground-color 05 background-color 03
             value "-Nome Fantasia".
          02 line 22 column 40 foreground-color 02 background-color 03
             highlight value "3".
          02 line 22 column 41 foreground-color 05 background-color 03
             value "-Razao Social".
          02 line 22 column 60 foreground-color 02 background-color 03
             highlight value "4".
          02 line 22 column 61 foreground-color 05 background-color 03
             value "-C.e.p.".
      *
       01 tela-03.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 22 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 22 column 09 foreground-color 05 background-color 03
             value "-Capital".
          02 line 22 column 25 foreground-color 02 background-color 03
             highlight value "2".
          02 line 22 column 26 foreground-color 05 background-color 03
             value "-Interior".
      *
       01 tela-04.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value " - Categorias".
      *
       01 tela-05.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "1".
          02 line 22 column 07 foreground-color 05 background-color 03
             value "-Ativo".
          02 line 22 column 14 foreground-color 02 background-color 03 
             highlight value "2".
          02 line 22 column 15 foreground-color 05 background-color 03
             value "-CLD Solicitacao".
          02 line 22 column 33 foreground-color 02 background-color 03 
             highlight value "3".
          02 line 22 column 34 foreground-color 05 background-color 03
             value "-CLD Debito".
          02 line 22 column 47 foreground-color 02 background-color 03 
             highlight value "4".
          02 line 22 column 48 foreground-color 05 background-color 03
             value "-Falencia".
          02 line 22 column 59 foreground-color 02 background-color 03 
             highlight value "5".
          02 line 22 column 60 foreground-color 05 background-color 03
             value "-Suspensao".
      *
       01 tela-06.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "A".
          02 line 22 column 07 foreground-color 05 background-color 03
             value "-Associados".
          02 line 22 column 24 foreground-color 02 background-color 03 
             highlight value "F".
          02 line 22 column 25 foreground-color 05 background-color 03
             value "-Afiliados".
      *
       01 tela-08.
          02 line 22 column 05 foreground-color 05 background-color 03
             pic x(66) from spaces.
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
       01 tela-09.
          02 line 22 column 05 foreground-color 05 background-color 03
             pic x(66) from spaces.
          02 line 22 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 22 column 12 foreground-color 02 background-color 03
             highlight value "I".
          02 line 22 column 13 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-10.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(66) from spaces.
          02 line 22 column 08 foreground-color 02 background-color 03
             highlight value "1".
          02 line 22 column 09 foreground-color 05 background-color 03
             value "-Sintetico".
          02 line 22 column 25 foreground-color 02 background-color 03
             highlight value "2".
          02 line 22 column 26 foreground-color 05 background-color 03
             value "-Analitico".
      *
       01 tela-mensagem-cad.
          02 line 22 column 05 foreground-color 07 background-color 01
             highlight pic x(66) from mensagem.
      *
       01 tela-erro-cad.
          02 line 22 column 05 beep reverse-video pic x(66) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 22 column 05 foreground-color 01 background-color 01
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
           move 10 to box-lin.
           move 70 to box-col-f.
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
       rot-move.
           perform rot-le-titular.
           move ab04-nome-a to cab-titular.
           move ab01-codigo to cab-codigo.
           move ab01-condicao to cab-condicao.
           if sele-ord not = 2
              move ab01-razao-social-a to cab-razao-social
           else 
              move ab01-nome-fantasia-a to cab-razao-social
           end-if.
           move ab01-endereco to cab-endereco.
           move ab01-cep to cab-cep.
           move ab01-uf to cab-uf.
           move ab01-cidade to cab-cidade.
           move ab01-ddd to cab-ddd.
           move ab01-telefone (01) to cab-telefone-1.
           move ab01-telefone (02) to cab-telefone-2.
           move ab01-telex to cab-telex.
           move ab01-fax to cab-fax.
           move ab01-ramal (01) to cab-ramal-1.
           move ab01-ramal (02) to cab-ramal-2.
           if sele-tipo = 2
              move ab01-sindicato to cab-sindicato
              move ab01-snea to cab-snea
              move ab01-embratur to cab-embratur
              move ab01-iata to cab-iata
              move ab01-cotal to cab-cotal
              move ab01-categoria to cab-categoria
              move ab01-situacao to cab-situacao
      *        move ab01-cgc to cab-cgc
              move ab01-data-alt to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to cab-alteracao
              move ab01-data-inc to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to cab-adesao
              if ab01-situacao not = 1
                 move ab01-data-exc to dias-corr
                 move 1 to opcao-data
                 perform rot-data
                 move data-disp to cab-cancelamento
              else
                 move spaces to cab-cancelamento
              end-if
           end-if.
      *
       rot-move-tit.
           move ab04-nome-a to cab-nome-t.
           move ab04-endereco to cab-endereco-t.
           move ab04-cidade to cab-cidade-t.
           move ab04-uf to cab-uf-t.
           move ab04-cep to cab-cep-t.
           move ab04-ddd to cab-ddd-t.
           move ab04-telefone to cab-telefone-t.
           move ab04-nascionalidade to cab-nascionalidade-t.
           move ab04-rg to cab-rg-t.
           move ab04-emissor to cab-emissor-t.
           move ab04-cpf to cab-cpf-t.
           move ab04-cargo to cab-cargo-t.
           if ab04-data-nasc not = 0
              move ab04-data-nasc to dias-corr
              move 1 to opcao-data
              perform rot-data
              move data-disp to cab-dt-nasc-t
           else
              move spaces to cab-dt-nasc-t
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
           read arqab01 next at end move 1 to erro.
           if ab01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
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
       rot-le-ab04.
           move 0 to erro.
           read arqab04 invalid key move 1 to erro.
           if ab04-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-ab04
           end-if.
      *
       rot-le-proximo-ab04.
           move 0 to erro.
           read arqab04 next at end move 1 to erro.
           if ab04-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo-ab04
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
       rot-le-titular.
          move low-value to ab04-chave.
          move ab01-codigo to ab04-codigo.
          move ab01-condicao to ab04-condicao.
          move 01 to ab04-titular.
          perform rot-le-ab04.
          if erro not = 0
             move spaces to ab04-nome-a
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
       rot-cabec.
           move param-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
           move 7 to linha.
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
           write reg-imp from tracos after 1 line.
      *
       err-categoria.
           move " Categoria nao cadastrada - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
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
           accept resposta at 2140 with auto foreground-color 01
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
           accept sele-ord at 1322 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-tipo.
           accept sele-tipo at 1422 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-condicao.
           accept sele-condicao at 1522 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-situacao.
           accept sele-situacao at 1622 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-ano.
           accept sele-ano at 1651 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cep-i.
           accept sele-cep-i at 1722 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cep-f.
           accept sele-cep-f at 1822 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-localidade.
           accept sele-localidade at 1922 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-categoria.
           accept sele-categoria at 2022 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-ord.
           display sele-ord at 1322 with foreground-color 15 
                   background-color 01.
      *
       dsp-tipo.
           display sele-tipo at 1422 with foreground-color 15 
                   background-color 01.
      *
       dsp-condicao.
           display sele-condicao-disp at 1522 with foreground-color 15 
                   background-color 01.
      *
       dsp-situacao.
           display sele-situacao-disp at 1622 with foreground-color 15 
                   background-color 01.
      *
       dsp-ano.
           display sele-ano-disp at 1651 with foreground-color 15 
                   background-color 01.
      *
       dsp-cep-i.
           display sele-cep-i-disp at 1722 with foreground-color 15 
                   background-color 01.
      *
       dsp-cep-f.
           display sele-cep-f-disp at 1822 with foreground-color 15 
                   background-color 01.
      *
       dsp-localidade.
           display sele-localidade-disp at 1922 with foreground-color 15 
                   background-color 01.
      *
       dsp-categoria.
           display sele-categoria at 2022 with foreground-color 15 
                   background-color 01.
           display sele-dcategoria at 2026 with foreground-color 15 
                   background-color 01.
      *
       dsp-categoria-todas.
           display "Todas" at 2022 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-ord.
           display limpa at 1322 with foreground-color 15 
                   background-color 01.
      *
       lmp-tipo.
           display limpa at 1422 with foreground-color 15 
                   background-color 01.
      *
       lmp-condicao.
           display limpa at 1522 with foreground-color 15 
                   background-color 01.
      *
       lmp-situacao.
           display limpa-10 at 1622 with foreground-color 15 
                   background-color 01.
      *
       lmp-ano.
           display limpa-10 at 1651 with foreground-color 15 
                   background-color 01.
      *
       lmp-cep-i.
           display limpa at 1722 with foreground-color 15 
                   background-color 01.
      *
       lmp-cep-f.
           display limpa at 1822 with foreground-color 15 
                   background-color 01.
      *
       lmp-localidade.
           display limpa at 1922 with foreground-color 15 
                   background-color 01.
      *
       lmp-categoria.
           display limpa at 2022 with foreground-color 15 
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
           if sele-ord not = 1 and 2 and 3 and 4
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
           move spaces to sele-condicao.
           perform lmp-condicao.
           perform acc-condicao.
           if escape-key = 1
              perform lmp-condicao
              go to lab-sele-02
           end-if.
           move sele-condicao to txt.
           perform rot-texto.
           if txt not = "A" and "F" and spaces
              go to lab-sele-03
           end-if.
           move txt to sele-condicao sele-condicao-disp.
           if sele-condicao = spaces
              move "Todos" to sele-condicao-disp
           end-if.
           perform dsp-condicao.
      *
       lab-sele-04.
           display tela-05.
           move 0 to sele-situacao.
           perform lmp-situacao.
           perform acc-situacao.
           if escape-key = 1
              perform lmp-situacao
              go to lab-sele-03
           end-if.
           if sele-situacao not = 1 and 2 and 3 and 4 and 5 and 0
              go to lab-sele-04
           end-if.
           if sele-situacao = 0
              move "Todos" to sele-situacao-disp
              perform dsp-situacao
           end-if.
           display tela-limpa-cad.
      *
       lab-sele-04-1.
           move 0 to sele-ano.
           perform lmp-ano.
           perform acc-ano.
           if escape-key = 1
              perform lmp-ano
              go to lab-sele-04
           end-if.
           if sele-ano = 0
              move "Todos" to sele-ano-disp
              perform dsp-ano
           end-if.
      *
       lab-sele-04-2.
           move 0 to sele-cep-i.
           perform lmp-cep-i.
           perform acc-cep-i.
           if escape-key = 1
              perform lmp-cep-i
              go to lab-sele-04-1
           end-if.
           if sele-cep-i = 0
              move "Inicial" to sele-cep-i-disp
              perform dsp-cep-i
              go to lab-sele-04-3
           end-if.
           move sele-cep-i to sele-cep-i-aux. 
           move sele-cep-i-aux to sele-cep-i-disp.
           perform dsp-cep-i.
      *
       lab-sele-04-3.
           move 0 to sele-cep-f.
           perform lmp-cep-f.
           perform acc-cep-f.
           if escape-key = 1
              perform lmp-cep-f
              go to lab-sele-04-2
           end-if.
           if sele-cep-f = 0
              move "Final" to sele-cep-f-disp
              move 99999999 to sele-cep-f
              perform dsp-cep-f
              move 0 to rotina-codigo
              go to lab-sele-05
           end-if.
           move sele-cep-f to sele-cep-f-aux. 
           move sele-cep-f-aux to sele-cep-f-disp.
           perform dsp-cep-f.
           move 0 to rotina-codigo.
      *
       lab-sele-05.
           display tela-03.
           move 0 to sele-localidade.
           perform lmp-localidade.
           perform acc-localidade.
           if escape-key = 1
              perform lmp-localidade
              go to lab-sele-04-3
           end-if.
           if sele-localidade not = 1 and 2 and 0
              go to lab-sele-05
           end-if.
           if sele-localidade = 0
              move "Todas" to sele-localidade-disp
           else
              move sele-localidade to sele-localidade-disp
           end-if.
           perform dsp-localidade.
           move 0 to rotina-codigo.
      *
       lab-sele-06.
           display tela-04.
           move rotina-codigo to sele-categoria.
           move spaces to sele-dcategoria.
           perform lmp-categoria.
           perform acc-categoria.
           if escape-key = 1
              perform lmp-categoria
              go to lab-sele-05
           end-if.
           if escape-key = 3
              move 1 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-sele-06
           end-if.
           if sele-categoria = 0
              perform dsp-categoria-todas
              go to lab-sele-07
           end-if.
           move 01 to wtab01-tipo.
           move sele-categoria to wtab01-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform err-categoria
              perform rot-keypress
              display tela-limpa-cad
              go to lab-sele-06
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to sele-dcategoria.
           perform dsp-categoria.
      *
       lab-sele-07.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-sele-06
           end-if.
           if resposta = "N"
              display tela-limpa-cad
              perform lmp-ord thru lmp-categoria
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-07
              end-if
           end-if.
           if sele-ord not = 4
              perform sec-impressao
           else
              sort arqsort on ascending key reg-sort
                   input procedure sec-ordenamento
                   output procedure sec-impressao-cep
           end-if.
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
           perform rot-open-ab01.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-ab04.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           move 99 to linha.
           move 0 to pagina.
           evaluate true
                  when sele-ord = 1
                       move low-values to ab01-chave
                       start arqab01 key is not less ab01-chave
                  when sele-ord = 2
                       move low-values to ab01-chave-1
                       start arqab01 key is not less ab01-chave-1
                  when sele-ord = 3
                       move low-values to ab01-chave-2
                       start arqab01 key is not less ab01-chave-2
           end-evaluate.
           display tela-09.
      *
       lab-imp-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if ab01-chave = high-values
              go to lab-imp-01
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-fim
           end-if.
           if sele-condicao not = spaces
              if ab01-condicao not = sele-condicao
                 go to lab-imp-01
              end-if
           end-if.
           if sele-situacao not = 0
              if ab01-situacao not = sele-situacao
                 go to lab-imp-01
              end-if
           end-if.
           if sele-categoria not = 0
              if ab01-categoria not = sele-categoria
                 go to lab-imp-01
              end-if
           end-if.
           if sele-localidade not = 0
              evaluate true
                       when sele-localidade = 1
                            if ab01-cep > 05901000
                               go to lab-imp-01
                            end-if
                       when sele-localidade = 2
                            if ab01-cep < 05901000
                               go to lab-imp-01
                            end-if
             end-evaluate
           end-if.
           if sele-ano not = 0
              if ab01-situacao = 1
                 move ab01-data-inc to dias-corr
                 move 1 to opcao-data
                 perform rot-data
              else
                 move ab01-data-exc to dias-corr
                 move 1 to opcao-data
                 perform rot-data
              end-if
              if sele-ano not = disp-ano
                 go to lab-imp-01
              end-if
           end-if.
           if ab01-cep < sele-cep-i or ab01-cep > sele-cep-f
              go to lab-imp-01
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
           if sele-tipo = 2
              write reg-imp from cab-07 after 1 line
              write reg-imp from cab-08 after 1 line
              write reg-imp from cab-09 after 1 line
              write reg-imp from cab-10 after 1 line
              add 4 to linha
           end-if.
           write reg-imp from tracos after 1 line.
           add 7 to linha.
           if sele-tipo = 2
              write reg-imp from cab-11 after 1 line
              write reg-imp from tracos after 1 line
              perform sec-titular
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
           perform rot-close-ab04.
           perform rot-close-ab01.
           exit.
      *
       sec-ordenamento section.
      *
       lab-ord-00.
           perform rot-open-ab01.
           if erro not = 0
              go to lab-ord-fim
           end-if.
           perform rot-open-ab04.
           if erro not = 0
              go to lab-ord-fim
           end-if.
           move low-values to ab01-chave.
           start arqab01 key is not less ab01-chave.
           display tela-09.
      *
       lab-ord-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-ord-fim
           end-if.
           if ab01-chave = high-values
              go to lab-ord-01
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-ord-fim
           end-if.
           move spaces to resposta.
           if sele-condicao not = spaces
              if ab01-condicao not = sele-condicao
                 go to lab-ord-01
              end-if
           end-if.
           if sele-situacao not = 0
              if ab01-situacao not = sele-situacao
                 go to lab-ord-01
              end-if
           end-if.
           if sele-categoria not = 0
              if ab01-categoria not = sele-categoria
                 go to lab-ord-01
              end-if
           end-if.
           if sele-localidade not = 0
              evaluate true
                       when sele-localidade = 1
                            if ab01-cep > 05901000
                               go to lab-ord-01
                            end-if
                       when sele-localidade = 2
                            if ab01-cep < 05901000
                               go to lab-ord-01
                            end-if
             end-evaluate
           end-if.
           if sele-ano not = 0
              if ab01-situacao = 1
                 move ab01-data-inc to dias-corr
                 move 1 to opcao-data
                 perform rot-data
              else
                 move ab01-data-exc to dias-corr
                 move 1 to opcao-data
                 perform rot-data
              end-if
              if sele-ano not = disp-ano
                 go to lab-imp-01
              end-if
           end-if.
           if ab01-cep < sele-cep-i or ab01-cep > sele-cep-f
              go to lab-ord-01
           end-if.
           move ab01-codigo to sort-codigo.
           move ab01-cep to sort-cep.
           move ab01-condicao to sort-condicao.
           release reg-sort.
           go to lab-ord-01.
      * 
       lab-ord-fim.
           exit.
      *
       sec-impressao-cep section.
      *
       lab-imp-cep-00.
           if resposta = "F"
              go to lab-imp-cep-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-imp-cep-fim
           end-if.
           display tela-09.
           move 99 to linha.
           move 0 to pagina.
      *
       lab-imp-cep-01.
           move 0 to erro.
           return arqsort at end move 1 to erro
                                 go to lab-imp-cep-fim
           end-return.
           move sort-codigo to ab01-codigo.
           move sort-condicao to ab01-condicao.
           perform rot-le-ab01.
           if erro not = 0
              go to lab-imp-cep-fim
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-imp-cep-fim
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
           if sele-tipo = 2
              write reg-imp from cab-07 after 1 line
              write reg-imp from cab-08 after 1 line
              write reg-imp from cab-09 after 1 line
              write reg-imp from cab-10 after 1 line
              add 4 to linha
           end-if.
           write reg-imp from tracos after 1 line.
           add 7 to linha.
           if sele-tipo = 2
              write reg-imp from cab-11 after 1 line
              write reg-imp from tracos after 1 line
              perform sec-titular
              if resposta = "F"
                 move 27 to kbd2
                 go to lab-imp-fim
              end-if
           end-if.
           go to lab-imp-cep-01.
      * 
       lab-imp-cep-fim.
           if kbd2 not = 27
              if sele-tipo = 1
                 write reg-imp from tracos after 1 line
              end-if
           end-if.
           if pagina not = 0
              write reg-imp from spaces after page
           end-if.
           perform rot-close-imp.
           perform rot-close-ab04.
           perform rot-close-ab01.
           exit.
      *

      *
       sec-titular section.
      *
       lab-tit-00.
           move low-values to ab04-chave.
           move ab01-codigo to ab04-codigo.
           move ab01-condicao to ab04-condicao.
           start arqab04 key is not less ab04-chave.
      *
       lab-tit-01.
           perform rot-le-proximo-ab04.
           if erro not = 0
              go to lab-tit-fim
           end-if.
           if ab04-chave = high-values
              go to lab-tit-fim
           end-if.
           if ab04-codigo not = ab01-codigo or
              ab04-condicao not = ab01-condicao
              go to lab-tit-fim
           end-if.
           perform rot-interrompe.
           if resposta = "F"
              move 27 to kbd2
              go to lab-tit-fim
           end-if.
           if linha > 56
              perform rot-cabec
           end-if.
           perform rot-move-tit.
           write reg-imp from cab-12 after 1 line.
           write reg-imp from cab-13 after 1 line.
           write reg-imp from cab-14 after 1 line.
           write reg-imp from cab-15 after 1 line.
           write reg-imp from cab-16 after 1 line.
           write reg-imp from cab-17 after 1 line.
           write reg-imp from tracos after 1 line.
           add 7 to linha.
           go to lab-tit-01.
      *
       lab-tit-fim.
           move 57 to linha.
           exit.
