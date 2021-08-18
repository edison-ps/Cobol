      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  PGFT02       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Impressao de ficha de Cobranca :                           *
      *                                                             *
      *  Data da ultima alteracao:    02/02/94     v1.00            *
      *                               01/06/95     v1.01 Form. Cont.*
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgft02.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqrc01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is rc01-chave
                  alternate record key is rc01-chave-1 with duplicates
                  alternate record key is rc01-chave-2 with duplicates
                  alternate record key is rc01-chave-3 with duplicates
                  file status is rc01-status.
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
       copy fdrc01.lib.
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
       01 rc01-status                  pic x(02) value "00".
       01 rc01-stat                    pic x(01) value "F".
      *
       01 nome-arq-rc01.
          02 rc01-dir                  pic x(03) value "RC2".
          02 filler                    pic x(01) value "\".
          02 rc01-nome                 pic x(08) value "ARQRC01A".
          02 filler                    pic x(01) value ".".
          02 rc01-ext                  pic x(03) value "DAT".
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
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGFT02".
          02 cb-versao                 pic x(06) value "v1.01 ".
      *
       01 limpa                        pic x(48) value spaces.
       01 limpa-aux                    pic x(05) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 sele-condicao             pic x(01) value spaces.
          02 sele-dcondicao            pic x(05) value spaces.
          02 sele-codigo-i             pic 9(05) value 0.
          02 sele-razao-i              pic x(40) value spaces.
          02 sele-codigo-f             pic 9(05) value 0.
          02 sele-razao-f              pic x(40) value spaces.
          02 sele-vencimento           pic 9(06) value 0.
          02 sele-vencimento-disp      pic x(08) value spaces.
          02 sele-portador             pic 9(03) value 0.
          02 sele-dportador            pic x(40) value spaces.
          02 sele-operacao             pic 9(03) value 0.
          02 sele-doperacao            pic x(40) value spaces.
          02 sele-obs                  pic x(30) value spaces.
          02 sele-obs-1                pic x(30) value spaces.
          02 sele-obs-2                pic x(30) value spaces.
          02 codigo-aux                pic 9(06) value 0.
      *
       01 documento.
          02 doc-codigo                pic 9(06) value 0.
          02 doc-filler                pic x(04) value spaces.
      * 
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       01 cab-usr.
          02 filler                    pic x(10) value "Usuario.:".
          02 cab-usuario               pic x(10) value spaces.
          02 filler                    pic x(02) value spaces.
          02 filler                    pic x(10) value "Data....:".
          02 cab-data                  pic x(08) value spaces.
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
       01 cab-1-8.
          02 filler                    pic x(01) value x"1b".
          02 filler                    pic x(01) value "0".
      *
       01 cab-1-6.
          02 filler                    pic x(01) value x"1b".
          02 filler                    pic x(01) value "2".
      *
       01 cab-01.
          02 filler                    pic x(61) value spaces.
          02 cab-vencimento            pic x(08) value spaces.
      *
       01 cab-02.
          02 filler                    pic x(50) value spaces.
      *       "ABAV - ASSOC. BRAS. DE AG. DE VIAG. DE SAO PAULO".
      *
       01 cab-03.
          02 cab-emissao               pic x(08) value spaces.
      *
       01 cab-04.
          02 filler                    pic x(56) value spaces.
          02 filler                    pic x(01) value "".
          02 filler                    pic x(05) value spaces.
          02 cab-valor                 pic zz.zzz.zzz.zz9,99.
          02 filler                    pic x(01) value "".
      *
       01 cab-05.
          02 cab-obs                   pic x(50) value spaces.
      *
       01 cab-05-1.
          02 cab-obs-1                 pic x(50) value spaces.
      *
       01 cab-05-2.
          02 cab-obs-2                 pic x(50) value spaces.
      *
       01 cab-06.
          02 filler                    pic x(01) value "".
          02 filler                    pic x(10) value spaces.
          02 cab-codigo                pic 9(06) value 0.
          02 filler                    pic x(01) value spaces.
          02 cab-razao                 pic x(40) value spaces.
          02 filler                    pic x(03) value spaces.
          02 filler                    pic x(08) value "C.G.C.:".
          02 cab-cgc                   pic 99.999.999/9999b99
             blank when zero.
          02 filler                    pic x(01) value "".
      *
       01 cab-07.
          02 filler                    pic x(01) value "".
          02 filler                    pic x(10) value spaces.
          02 cab-endereco              pic x(40) value spaces.
          02 filler                    pic x(01) value "".
      *
       01 cab-08.
          02 filler                    pic x(01) value "".
          02 filler                    pic x(10) value spaces.
          02 cab-cep                   pic 99999b999 value 0.
          02 filler                    pic x(03) value spaces.
          02 cab-cidade                pic x(20) value spaces.
          02 filler                    pic x(03) value spaces.
          02 cab-uf                    pic x(02) value spaces.
          02 filler                    pic x(10) value spaces.
          02 cab-documento             pic x(10) value spaces.
          02 filler                    pic x(01) value "".
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
          02 line 11 column 55 foreground-color 06 background-color 01
             highlight value "Fichas de Cobranca".
          02 line 12 column 06 foreground-color 06 background-color 01
             highlight value "Condicao.......:".
          02 line 13 column 06 foreground-color 06 background-color 01
             highlight value "Codigo Inicial.:".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "Codigo Final...:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Vencimento.....:".
          02 line 16 column 06 foreground-color 06 background-color 01
             highlight value "Portador.......:".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "Operacao.......:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "Observacao.....:".
      *
       01 tela-02.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value " - Portadores".
      *
       01 tela-03.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value " - Operacoes".
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
       01 tela-06.
          02 line 22 column 05 foreground-color 05 background-color 03
             pic x(68) from spaces.
          02 line 22 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 22 column 12 foreground-color 02 background-color 03
             highlight value "I".
          02 line 22 column 13 foreground-color 05 background-color 03
             value ") para interromper".
      *
       01 tela-07.
          02 line 22 column 05 foreground-color 05 background-color 03
             highlight pic x(68) from mensagem.
          02 line 22 column 05 foreground-color 05 background-color 03
             value "(".
          02 line 22 column 06 foreground-color 02 background-color 03
             highlight value "I".
          02 line 22 column 07 foreground-color 05 background-color 03
             value ")mprimir   (".
          02 line 22 column 19 foreground-color 02 background-color 03
             highlight value "P".
          02 line 22 column 20 foreground-color 05 background-color 03
             value ")osicionar   (".
          02 line 22 column 34 foreground-color 02 background-color 03
             highlight value "E".
          02 line 22 column 35 foreground-color 05 background-color 03
             value ")ncerrar".
      *
       01 tela-08.
          02 line 22 column 05 foreground-color 05 background-color 03
             pic x(68) from spaces.
          02 line 22 column 05 foreground-color 05 background-color 03
             value "Tecle (".
          02 line 22 column 12 foreground-color 02 background-color 03
             highlight value "I".
          02 line 22 column 13 foreground-color 05 background-color 03
             value ") para interromper".
          02 line 22 column 35 foreground-color 05 background-color 03
             value "(".
          02 line 22 column 36 foreground-color 02 background-color 03
             highlight value "Enter".
          02 line 22 column 41 foreground-color 05 background-color 03
             value ") para imprimir".
      *
       01 tela-09.
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
       rot-imprimir.
           move sele-obs to cab-obs.
           move sele-obs-1 to cab-obs-1.
           move sele-obs-2 to cab-obs-2.
           move sele-vencimento-disp to cab-vencimento.
           move rc01-valor to cab-valor.
           move rc01-emissao to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-emissao.
           move ab01-razao-social-a to cab-razao.
           move rc01-codigo to codigo-aux.
           if rc01-condicao = "F"
              add 900000 to codigo-aux 
           end-if.
           move codigo-aux to cab-codigo.
           move ab01-cgc to cab-cgc.
           move ab01-endereco to cab-endereco.
           move ab01-cep to cab-cep.
           move ab01-cidade to cab-cidade.
           move ab01-uf to cab-uf.
           move rc01-documento to cab-documento.
           write reg-imp from cab-1-8.
           write reg-imp from cab-01 after 3 lines.
           write reg-imp from cab-02 after 2 lines.
           write reg-imp from cab-03 after 2 lines.
           write reg-imp from cab-04 after 2 lines.
           write reg-imp from cab-05 after 3 lines.
           write reg-imp from cab-05-1 after 1 lines.
           write reg-imp from cab-05-2 after 1 lines.
           write reg-imp from cab-06 after 7 lines.
           write reg-imp from cab-07 after 1 lines.
           write reg-imp from cab-08 after 1 lines.
           write reg-imp from spaces after 8 lines.
           write reg-imp from cab-1-6 before 0 lines.
      *
       rot-posicionar.
           move all "X" to cab-obs cab-obs-1 cab-obs-2.
           move "99/99/99" to cab-vencimento.
           move 9999999999999 to cab-valor.
           move "99/99/99" to cab-emissao.
           move all "X" to cab-razao.
           move 999999 to cab-codigo.
           move 99999999999999 to cab-cgc.
           move all "X" to cab-endereco.
           move 99999999 to cab-cep.
           move all "X" to cab-cidade.
           move "XX" to cab-uf.
           move all "X" to cab-documento.
           write reg-imp from cab-1-8.
           write reg-imp from cab-01 after 3 lines.
           write reg-imp from cab-02 after 2 lines.
           write reg-imp from cab-03 after 2 lines.
           write reg-imp from cab-04 after 2 lines.
           write reg-imp from cab-05 after 3 lines.
           write reg-imp from cab-05-1 after 1 lines.
           write reg-imp from cab-05-2 after 1 lines.
           write reg-imp from cab-06 after 7 lines.
           write reg-imp from cab-07 after 1 lines.
           write reg-imp from cab-08 after 1 lines.
           write reg-imp from spaces after 8 lines.
           write reg-imp from cab-1-6 before 0 lines.
      *
       rot-open-rc01.
           move 0 to erro.
           if rc01-stat = "F"
              open i-o arqrc01
              if rc01-status not = "00"
                 move 
                 " Erro de abertura no ARQRC01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to rc01-stat
               end-if
           end-if.
      *
       rot-close-rc01.
           if rc01-stat = "A"
              close arqrc01
              move "F" to rc01-stat
           end-if.
      *
       rot-erro-leitura-rc01.
           move " Erro de leitura - ARQRC01A.DAT - Tecle <Enter>" to
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
           move 11 to rotina-lin.
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
       rot-le-rc01.
           move 0 to erro.
           read arqrc01 next at end move 1 to erro.
           if rc01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-rc01
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
       rot-interrompe.
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
       rot-data-i.
           move " Data invalida - Tecle <Enter>" to
           mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-limpa-cad.
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
       rot-pesq-associado.
           perform rot-close-ab01.
           move 08 to rotina-col-cod.
           move 10 to rotina-lin-cod.
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
           accept resposta at 1968 with auto foreground-color 01
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
       acc-condicao.
           accept sele-condicao at 1223 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-codigo-i.
           accept sele-codigo-i at 1323 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-codigo-f.
           accept sele-codigo-f at 1423 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-vencimento.
           accept sele-vencimento at 1523 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-portador.
           accept sele-portador at 1623 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-operacao.
           accept sele-operacao at 1723 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-obs.
           accept sele-obs at 1823 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-obs-1.
           accept sele-obs-1 at 1923 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-obs-2.
           accept sele-obs-2 at 2023 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-condicao.
           display sele-dcondicao at 1223 with foreground-color 15 
                   background-color 01.
      *
       dsp-codigo-i.
           display sele-codigo-i at 1323 with foreground-color 15 
                   background-color 01.
           display sele-razao-i at 1330 with foreground-color 15 
                   background-color 01.

      *
       dsp-codigo-f.
           display sele-codigo-f at 1423 with foreground-color 15 
                   background-color 01.
           display sele-razao-f at 1430 with foreground-color 15 
                   background-color 01.
      *
       dsp-vencimento.
           display sele-vencimento-disp at 1523 with foreground-color 15
                   background-color 01.
      *
       dsp-portador.
           display sele-portador at 1623 with foreground-color 15 
                   background-color 01.
           display sele-dportador at 1627 with foreground-color 15 
                   background-color 01.
      *
       dsp-operacao.
           display sele-operacao at 1723 with foreground-color 15 
                   background-color 01.
           display sele-doperacao at 1727 with foreground-color 15 
                   background-color 01.
      *
       dsp-obs.
           display sele-obs at 1823 with foreground-color 15 
                   background-color 01.
      *
       dsp-obs-1.
           display sele-obs-1 at 1923 with foreground-color 15 
                   background-color 01.
      *
       dsp-obs-2.
           display sele-obs-2 at 2023 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-condicao.
           display limpa at 1223 with foreground-color 15 
                   background-color 01.
      *
       lmp-codigo-i.
           display limpa at 1323 with foreground-color 15 
                   background-color 01.
      *
       lmp-codigo-f.
           display limpa at 1423 with foreground-color 15 
                   background-color 01.
      *
       lmp-vencimento.
           display limpa at 1523 with foreground-color 15 
                   background-color 01.
      *
       lmp-portador.
           display limpa at 1623 with foreground-color 15 
                   background-color 01.
      *
       lmp-operacao.
           display limpa at 1723 with foreground-color 15 
                   background-color 01.
      *
       lmp-obs.
           display limpa at 1823 with foreground-color 15 
                   background-color 01.
      *
       lmp-obs-1.
           display limpa at 1923 with foreground-color 15 
                   background-color 01.
      *
       lmp-obs-2.
           display limpa at 2023 with foreground-color 15 
                   background-color 01.
      *
       sec-selecao section.
      *
       lab-sele-00.
           display tela-limpa-cad.
           if param-prioridade < 5
              perform display-erro-usr
              go to lab-sele-fim
           end-if.
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
           display tela-04.
           move spaces to sele-condicao.
           perform lmp-condicao.
           perform acc-condicao.
           if escape-key = 1 
              go to lab-sele-fim
           end-if.
           move sele-condicao to txt.
           perform rot-texto.
           if txt not = "A" and "F" and space
              go to lab-sele-01
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
       lab-sele-02.
           display tela-09.
           move rotina-codigo-cod to sele-codigo-i.
           move spaces to sele-razao-i.
           perform lmp-codigo-i.
           perform acc-codigo-i.
           if escape-key = 1
              perform lmp-codigo-i
              display tela-limpa-cad
              go to lab-sele-01
           end-if.
           if escape-key = 3
              perform rot-pesq-associado
              go to lab-sele-02
           end-if.
           if sele-codigo-i = 0
              go to lab-sele-03
           end-if.
           if sele-condicao not = spaces
              move sele-codigo-i to ab01-codigo
              move sele-condicao to ab01-condicao
              perform rot-le-ab01
              if erro not = 0
                 perform rot-erro-codigo
                 go to lab-sele-02
              end-if
              move ab01-razao-social-a to sele-razao-i
           end-if.
           perform dsp-codigo-i.
           move 0 to rotina-codigo-cod.
      *
       lab-sele-03.
           display tela-09.
           move rotina-codigo-cod to sele-codigo-f.
           move spaces to sele-razao-f.
           perform lmp-codigo-f.
           perform acc-codigo-f.
           if escape-key = 1
              move 0 to rotina-codigo-cod
              perform lmp-codigo-f
              go to lab-sele-02
           end-if.
           if escape-key = 3
              perform rot-pesq-associado
              go to lab-sele-03
           end-if.
           if sele-codigo-f = 0
              move 99999 to sele-codigo-f
              perform dsp-codigo-f
              display tela-limpa-cad
              go to lab-sele-04
           end-if.
           if sele-codigo-f < sele-codigo-i
              perform rot-codigo-f-m
              go to lab-sele-03
           end-if.
           if sele-condicao not = spaces
              move sele-codigo-f to ab01-codigo
              move sele-condicao to ab01-condicao
              perform rot-le-ab01
              if erro not = 0
                 perform rot-erro-codigo
                 go to lab-sele-03
              end-if
              move ab01-razao-social-a to sele-razao-f
           end-if.
           perform dsp-codigo-f.
           display tela-limpa-cad.
      *
       lab-sele-04.
           perform lmp-vencimento.
           perform acc-vencimento.
           if escape-key = 1
              move 0 to rotina-codigo-cod
              perform lmp-vencimento
              go to lab-sele-03
           end-if.
           if sele-vencimento = 0
              move "C/APRES." to sele-vencimento-disp
              perform dsp-vencimento
              move 0 to rotina-codigo
              go to lab-sele-05
           end-if.
           move sele-vencimento to data-aux.
           move dia-aux to dia-euro.
           move mes-aux to mes-euro.
           move ano-aux to ano-euro.
           move 4 to opcao-data. 
           perform rot-data.
           if return-code not = 0
              perform rot-data-i
              go to lab-sele-04
           end-if.
           move data-disp to sele-vencimento-disp.
           move dias-corr to sele-vencimento
           perform dsp-vencimento.
           move 0 to rotina-codigo.
      *
       lab-sele-05.
           move rotina-codigo to sele-portador.
           display tela-02.
           perform lmp-portador.
           perform acc-portador.
           if escape-key = 1
              if sele-vencimento not = 0
                 move sele-vencimento to dias-corr
                 move 1 to opcao-data
                 perform rot-data
                 move dia-euro to dia-aux
                 move mes-euro to mes-aux
                 move ano-euro to ano-aux
                 move data-aux to sele-vencimento
              end-if
              perform lmp-portador
              display tela-limpa-cad
              go to lab-sele-04
           end-if.
           if escape-key = 3
              move 4 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-sele-05
           end-if.
           if sele-portador = 0
              go to lab-sele-05
           end-if.
           move 04 to wtab01-tipo.
           move sele-portador to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-portador
              go to lab-sele-05
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to sele-dportador.
           perform dsp-portador.
           display tela-limpa-cad.
           move 0 to rotina-codigo.
      *
       lab-sele-06.
           move rotina-codigo to sele-operacao.
           display tela-03.
           perform lmp-operacao.
           perform acc-operacao.
           if escape-key = 1
              move 0 to rotina-codigo
              perform lmp-operacao
              go to lab-sele-05
           end-if.
           if escape-key = 3
              move 5 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-sele-06
           end-if.
           if sele-operacao = 0
              go to lab-sele-06
           end-if.
           move 05 to wtab01-tipo.
           move sele-operacao to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-operacao
              go to lab-sele-06
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to sele-doperacao.
           perform dsp-operacao.
           display tela-limpa-cad.
      *
       lab-sele-07.
      *     move spaces to sele-obs.
           perform lmp-obs.
           perform acc-obs.
           if escape-key = 1
              perform lmp-obs
              go to lab-sele-06
           end-if.
           if sele-obs = spaces 
              go to lab-sele-10
           end-if.
      *
       lab-sele-08.
      *     move spaces to sele-obs-1.
           perform lmp-obs-1.
           perform acc-obs-1.
           if escape-key = 1
              perform lmp-obs-1
              go to lab-sele-07
           end-if.
           if sele-obs-1 = spaces 
              go to lab-sele-10
           end-if.
      *
       lab-sele-09.
      *     move spaces to sele-obs-2.
           perform lmp-obs-2.
           perform acc-obs-2.
           if escape-key = 1
              perform lmp-obs-2
              go to lab-sele-08
           end-if.
      *
       lab-sele-10.
           move "Confirma (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              if sele-obs-2 not = spaces
                 go to lab-sele-09
              else
                 if sele-obs-1 not = spaces
                    go to lab-sele-08
                 else
                    go to lab-sele-07
                 end-if
              end-if
           end-if.
           if resposta = "N"
              perform lmp-condicao thru lmp-obs-2
              display tela-limpa-cad
              go to lab-sele-01
           else
              if resposta not = "S"
                 go to lab-sele-10
              end-if
           end-if.
           display tela-limpa-cad.
           perform sec-impressao.
           perform lmp-condicao thru lmp-obs-2.
           display tela-limpa-cad.
           display tela-limpa.
           move data-aux to sele-vencimento.
           go to lab-sele-01.
      *
       lab-sele-fim.
           perform rot-close-tabl.
           perform rot-close-ab01.
           exit.
      *
       sec-impressao section.
      *
       lab-imp-00.
           perform rot-open-rc01.
           if erro not = 0
              go to lab-sele-fim
           end-if.
           perform rot-open-imp.
           if erro not = 0
              go to lab-sele-fim
           end-if.
           display tela-07.
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
           display tela-06.
           move 0 to erro.
           move sele-vencimento to rc01-vencimento.
           start arqrc01 key is not less rc01-chave-2 invalid key 
                  move 1 to erro.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           display tela-06.
      *
       lab-imp-02.
           perform rot-interrompe.
           if resposta = "F"
              go to lab-imp-fim
           end-if.
           perform rot-le-rc01.
           if erro not = 0
              go to lab-imp-fim
           end-if.
           if rc01-chave = low-values
              go to lab-imp-02
           end-if.
           if rc01-vencimento not = sele-vencimento
              go to lab-imp-fim
           end-if.
           if rc01-codigo > sele-codigo-f or rc01-codigo < sele-codigo-i
              go to lab-imp-02
           end-if.
           if sele-condicao not = spaces
              if rc01-condicao not = sele-condicao
                 go to lab-imp-02
              end-if
           end-if.
           if rc01-portador not = sele-portador
              go to lab-imp-02
           end-if.
           if rc01-operacao not = sele-operacao
              go to lab-imp-02
           end-if.
           move rc01-codigo to ab01-codigo.
           move rc01-condicao to ab01-condicao.
           perform rot-le-ab01.
           if erro not = 0
              go to lab-imp-02
           end-if.
      *     display tela-08.
      *     perform accept-resposta-cad.
      *     if escape-key = 1
      *        go to lab-imp-fim
      *     end-if.
      *     if resposta = "I"
      *        display tela-05
      *        perform until kbd2 = 67 or 99 or 70 or 102
      *                perform rot-keypress
      *        end-perform
      *        if kbd2 = 70 or 102
      *           go to lab-imp-fim
      *        end-if
      *     end-if.
           perform rot-imprimir.
           go to lab-imp-02.
      *
       lab-imp-fim.
           perform rot-close-imp.
           perform rot-close-rc01.
           exit.