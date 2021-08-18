      ***************************************************************
      *                                                             *
      *  E P S - S O F T                          :::  PGCD01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Manutencao do cadastro :                                   *
      *                                                             *
      *  Data da ultima alteracao:    26/10/93     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgcd01.
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
       data division.
       file section.
      *    
       copy fdcd01.lib.
      *    
       copy fdcd02.lib.
      *
       copy fdtabl.lib.
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
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGCD01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa-45                     pic x(45) value spaces.
       01 limpa-15       	       pic x(15) value spaces.
       01 limpa-08                     pic x(08) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-empresa                 pic x(01) value "N".
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
      *
       01 campos.
          02 codigo                    pic 9(05) value 0.
          02 nome-fantasia             pic x(40) value spaces.
          02 nome-fantasia-a           pic x(40) value spaces.
          02 razao-social              pic x(40) value spaces.
          02 razao-social-a            pic x(40) value spaces.
          02 categoria                 pic 9(03) value 0.
          02 dcategoria                pic x(30) value spaces.
          02 flag-cgcpf                pic x(01) value spaces.
          02 cgc                       pic 9(14) value 0.
          02 cgc-aux                   pic 99.999.999/9999b99.
          02 cpf                       pic 9(11) value 0.
          02 cpf-aux                   pic 999.999.999b99bbbb.
          02 endereco                  pic x(40) value spaces.
          02 cidade                    pic x(15) value spaces.
          02 uf                        pic x(02) value spaces.
          02 cep                       pic 9(08) value 0.
          02 endereco-cbr              pic x(40) value spaces.
          02 cidade-cbr                pic x(15) value spaces.
          02 uf-cbr                    pic x(02) value spaces.
          02 cep-cbr                   pic 9(08) value 0.
          02 ddd                       pic 9(04) value 0.
          02 telefone                  pic x(08) value spaces.
          02 fax                       pic x(08) value spaces.
          02 telex                     pic x(08) value spaces.
          02 ie                        pic 9(12) value 0
             blank when zero.
      *
       01 campos-c.
          02 c-contato                 pic 9(02) value 0.
          02 c-contato-aux             pic 9(02) value 0.
          02 c-nome                    pic x(40) value spaces.
          02 c-nome-a                  pic x(40) value spaces.
          02 c-controle                pic x(47) value spaces.
          02 c-flag                    pic 9(02) value 0.
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
          02 line 07 column 06 foreground-color 06 background-color 01
             highlight value "Codigo........:".
          02 line 08 column 06 foreground-color 06 background-color 01
             highlight value "Nome Fantasia.:".
          02 line 09 column 06 foreground-color 06 background-color 01
             highlight value "Razao Social..:".
          02 line 10 column 06 foreground-color 06 background-color 01
             highlight value "Categoria.....:".
          02 line 12 column 06 foreground-color 06 background-color 01
             highlight value "Endereco......:".
          02 line 13 column 06 foreground-color 06 background-color 01
             highlight value "UF............:".
          02 line 13 column 29 foreground-color 06 background-color 01
             highlight value "Cidade.:".
          02 line 13 column 56 foreground-color 06 background-color 01
             highlight value "C.E.P.:".
          02 line 14 column 06 foreground-color 06 background-color 01
             highlight value "DDD...........:".
          02 line 14 column 38 foreground-color 06 background-color 01
             highlight value "Telefone......:".
          02 line 15 column 06 foreground-color 06 background-color 01
             highlight value "Fax...........:".
          02 line 15 column 38 foreground-color 06 background-color 01
             highlight value "Telex.........:".
          02 line 16 column 05 foreground-color 07 background-color 02
             pic x(68) from spaces.
          02 line 16 column 04 foreground-color 07 background-color 01
             highlight value "�".
          02 line 16 column 05 foreground-color 06 background-color 01
             highlight value 
             "컴컴컴컴컴컴컴컴컴컴컴컴컴 Dados de Cobranca 컴컴컴컴컴컴�
      -      "컴컴컴컴컴".
          02 line 16 column 73 foreground-color 07 background-color 01
             highlight value "�".
          02 line 17 column 06 foreground-color 06 background-color 01
             highlight value "Endereco......:".
          02 line 18 column 06 foreground-color 06 background-color 01
             highlight value "UF............:".
          02 line 18 column 29 foreground-color 06 background-color 01
             highlight value "Cidade.:".
          02 line 18 column 56 foreground-color 06 background-color 01
             highlight value "C.E.P.:".
          02 line 19 column 06 foreground-color 06 background-color 01
             highlight value "Pessoa (F/J)..:".
          02 line 19 column 38 foreground-color 06 background-color 01
             highlight value "CGC/CPF.:".
          02 line 20 column 06 foreground-color 06 background-color 01
             highlight value "I.E...........:".
      *
       01 tela-02.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 22 column 07 foreground-color 05 background-color 03
             value "-Help".
          02 line 22 column 15 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 22 column 17 foreground-color 05 background-color 03
             value "-Consultas".
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
       01 tela-03.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 22 column 08 foreground-color 05 background-color 03
             value "-Help".
          02 line 22 column 16 foreground-color 02 background-color 03
             highlight value "C".
          02 line 22 column 17 foreground-color 05 background-color 03
             value "odigo".
          02 line 22 column 26 foreground-color 02 background-color 03
             highlight value "R".
          02 line 22 column 27 foreground-color 05 background-color 03
             value "azao".
          02 line 22 column 37 foreground-color 02 background-color 03
             highlight value "N".
          02 line 22 column 38 foreground-color 05 background-color 03
             value "ome".
          02 line 22 column 46 foreground-color 05 background-color 03
             value "c tegoria".
          02 line 22 column 47 foreground-color 02 background-color 03
             highlight value "A".
          02 line 22 column 58 foreground-color 05 background-color 03
             value "c c/cpf".
          02 line 22 column 59 foreground-color 02 background-color 03
             highlight value "G".
      *
       01 tela-04.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value "-Alt".
          02 line 22 column 14 foreground-color 02 background-color 03 
             highlight value "F3".
          02 line 22 column 16 foreground-color 05 background-color 03
             value "-Exc".
          02 line 22 column 22 foreground-color 02 background-color 03 
             highlight value "F4".
          02 line 22 column 24 foreground-color 05 background-color 03
             value "-Cont".
          02 line 22 column 31 foreground-color 02 background-color 03
             highlight value "Home".
          02 line 22 column 35 foreground-color 05 background-color 03
             value "-Inic".
          02 line 22 column 42 foreground-color 02 background-color 03
             highlight value "End".
          02 line 22 column 45 foreground-color 05 background-color 03
             value "-Fim".
          02 line 22 column 51 foreground-color 02 background-color 03
             highlight value "PgDown".
          02 line 22 column 57 foreground-color 05 background-color 03
             value "-Prox".
          02 line 22 column 64 foreground-color 02 background-color 03
             highlight value "PgUp".
          02 line 22 column 68 foreground-color 05 background-color 03
             value "-Ant".
      *
       01 tela-05.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value "-Categorias".
      *
       01 tela-06.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-07.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 22 column 08 foreground-color 05 background-color 03
             value "-Estados".
      *
       01 tela-08.
          02 line 22 column 05 foreground-color 02 background-color 03
             highlight pic x(68) from spaces.
          02 line 22 column 06 foreground-color 02 background-color 03 
             highlight value "F".
          02 line 22 column 07 foreground-color 05 background-color 03
             value "isica".
          02 line 22 column 14 foreground-color 02 background-color 03 
             highlight value "J".
          02 line 22 column 15 foreground-color 05 background-color 03
             value "uridica".
      *
       01 tela-09.
          02 line 06 column 65 foreground-color 06 background-color 01
             highlight value "Inclusao".
      *
       01 tela-10.
          02 line 06 column 65 foreground-color 06 background-color 01
             highlight value "Consulta".
      *
       01 tela-12.
          02 line 12 column 09 foreground-color 06 background-color 01
             highlight value "Contato..:".
          02 line 13 column 09 foreground-color 06 background-color 01
             highlight value "Nome.....:".
      *
       01 tela-13.
          02 line 11 column 63 foreground-color 06 background-color 01
             highlight value "Inclusao".
      *
       01 tela-14.
          02 line 11 column 63 foreground-color 06 background-color 01
             highlight value "Consulta".
      *
       01 tela-15.
          02 line 15 column 08 foreground-color 02 background-color 03
             highlight pic x(63) from spaces.
          02 line 15 column 09 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 15 column 11 foreground-color 05 background-color 03
             value "-Help".
          02 line 15 column 19 foreground-color 02 background-color 03
             highlight value "F2".
          02 line 15 column 21 foreground-color 05 background-color 03
             value "-Consultas".
      *
       01 tela-16.
          02 line 15 column 08 foreground-color 02 background-color 03
             highlight pic x(63) from spaces.
          02 line 15 column 09 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 15 column 11 foreground-color 05 background-color 03
             value " - Estados".
      *
       01 tela-17.
          02 line 15 column 08 foreground-color 02 background-color 03
             highlight pic x(63) from spaces.
          02 line 15 column 08 foreground-color 02 background-color 03
             highlight value "Registro gravado - Tecle <Enter>".
      *
       01 tela-18.
          02 line 15 column 08 foreground-color 02 background-color 03
             highlight pic x(63) from spaces.
          02 line 15 column 10 foreground-color 02 background-color 03
             highlight value "F1".
          02 line 15 column 12 foreground-color 05 background-color 03
             value "-Help".
          02 line 15 column 20 foreground-color 02 background-color 03
             highlight value "C".
          02 line 15 column 21 foreground-color 05 background-color 03
             value "ontato".
          02 line 15 column 31 foreground-color 02 background-color 03
             highlight value "N".
          02 line 15 column 32 foreground-color 05 background-color 03
             value "ome".
      *
       01 tela-19.
          02 line 15 column 08 foreground-color 02 background-color 03
             highlight pic x(63) from spaces.
          02 line 15 column 09 foreground-color 02 background-color 03 
             highlight value "F2".
          02 line 15 column 11 foreground-color 05 background-color 03
             value "-Alt".
          02 line 15 column 17 foreground-color 02 background-color 03 
             highlight value "F3".
          02 line 15 column 19 foreground-color 05 background-color 03
             value "-Exc".
          02 line 15 column 26 foreground-color 02 background-color 03
             highlight value "Home".
          02 line 15 column 30 foreground-color 05 background-color 03
             value "-Inic".
          02 line 15 column 38 foreground-color 02 background-color 03
             highlight value "End".
          02 line 15 column 41 foreground-color 05 background-color 03
             value "-Fim".
          02 line 15 column 47 foreground-color 02 background-color 03
             highlight value "PgDown".
          02 line 15 column 53 foreground-color 05 background-color 03
             value "-Prox".
          02 line 15 column 61 foreground-color 02 background-color 03
             highlight value "PgUp".
          02 line 15 column 65 foreground-color 05 background-color 03
             value "-Ant".
      *
       01 tela-mensagem-cad-c.
          02 line 15 column 08 foreground-color 07 background-color 01
             highlight pic x(63) from mensagem.
      *
       01 tela-erro-cad-c.
          02 line 15 column 08 beep reverse-video pic x(63) from 
             mensagem.
      *
       01 tela-limpa-cad-c.
          02 line 15 column 08 foreground-color 01 background-color 01
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
           move 03 to box-col.
           move 04 to box-lin.
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
       rot-move-cd01.
           move codigo to cd01-codigo.
           move nome-fantasia to cd01-chave-1.
           move nome-fantasia-a to cd01-nome-fantasia-a.
           move razao-social to cd01-chave-2.
           move razao-social-a to cd01-razao-social-a.
           move categoria to cd01-categoria.
           move razao-social to cd01-razao-social in cd01-chave-3.
           move flag-cgcpf to cd01-flag-cgcpf.
           if flag-cgcpf = "J"
              move cgc to cd01-cgcpf
           else
              move cpf to cd01-cgcpf
           end-if.
           move endereco to cd01-endereco.
           move cidade to cd01-cidade.
           move uf to cd01-uf.
           move cep to cd01-cep.
           move ddd to cd01-ddd.
           move endereco-cbr to cd01-endereco-cbr.
           move cidade-cbr to cd01-cidade-cbr.
           move uf-cbr to cd01-uf-cbr.
           move cep-cbr to cd01-cep-cbr.           
           move telefone to cd01-telefone.
           move telex to cd01-telex.
           move fax to cd01-fax.
           move ie to cd01-ie.
           move param-usr to cd01-usuario.
           move param-data to cd01-data.
      *
       rot-move-cd02.
           move cd01-codigo to cd02-codigo.
           move c-contato to cd02-contato.
           move c-nome to cd02-nome-c.
           move c-nome-a to cd02-nome-a.
           move param-usr to cd02-usuario.
           move param-data to cd02-data.
      *
       rot-move-campos.
           move cd01-codigo to codigo.
           move cd01-nome-fantasia-a  to nome-fantasia nome-fantasia-a.
           move cd01-categoria to categoria.
           move cd01-razao-social-a to razao-social razao-social-a.
           move cd01-flag-cgcpf to flag-cgcpf.
           if cd01-flag-cgcpf = "J"
              move cd01-cgcpf to cgc
           else
              move cd01-cgcpf to cpf
           end-if.
           move cd01-endereco to endereco.
           move cd01-cidade to cidade.
           move cd01-uf to uf.
           move cd01-cep to cep.
           move cd01-ddd to ddd.
           move cd01-endereco-cbr to endereco-cbr.
           move cd01-cidade-cbr to cidade-cbr.
           move cd01-uf-cbr to uf-cbr.
           move cd01-cep-cbr to cep-cbr.           
           move cd01-telefone to telefone.
           move cd01-telex to telex.
           move cd01-fax to fax.
           move cd01-ie to ie.
           move cd01-usuario to cab-usuario.
           move cd01-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
      *
       rot-move-campos-cont.
           move cd02-contato to c-contato.
           move cd02-nome-a to c-nome.
           move cd02-usuario to cab-usuario.
           move cd02-data to dias-corr.
           move 1 to opcao-data.
           perform rot-data.
           move data-disp to cab-data.
      *
       rot-le-tabl.
           move 0 to erro.
           read arqtabl invalid key move 1 to erro.
           if tabl-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-tabl.
      *
       rot-le-cd01-2.
           move 0 to erro.
           read arqcd01 key cd01-chave-2 invalid key move 1 to erro.
           if cd01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-cd01-2.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqcd01 key is equal cd01-chave invalid key
                 move 1 to erro
                 perform rot-erro-leitura-cd01
           end-start.
      *
       rot-le-cd01-lock.
           move 0 to erro.
           read arqcd01 next. 
           if cd01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-cd01-lock
           end-if.
           read arqcd01 with kept lock.
      *
       rot-le-anterior.
           move 0 to erro.
           read arqcd01 previous at end move 1 to erro.
           if cd01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-anterior
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
       rot-nome-cad.
           move " Nome ja cadastrado - Confirma (S) (N) ?" to
           mensagem.
           display tela-erro-cad.
           perform accept-resposta-cad.
           display tela-limpa-cad.
           if resposta not = "S" and "N"
              go to rot-nome-cad
           end-if.
      *
       rot-inic-arquivo.
           perform lmp-codigo thru lmp-ie.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo.
           perform lmp-codigo thru lmp-ie.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-erro-categoria.
           move " Categoria nao cadastrada - Tecle <Enter>" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
           display tela-05.
      *
       rot-erro-estado.
            move " Estado nao cadastrado - Tecle <Enter>" to mensagem.
            display tela-erro-cad.
            perform rot-keypress.
            display tela-08.
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
           perform dsp-codigo thru dsp-ie.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
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
       rot-le-cd02-next.
           move 0 to erro.
           read arqcd02 next at end move 1 to erro.
           if cd02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-cd02-next
           end-if.
      *
       rot-le-cd02-prev.
           move 0 to erro.
           read arqcd02 previous at end move 1 to erro.
           if cd02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-cd02-prev
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
       rot-ponteiro-cd02.
           move 0 to erro.
           start arqcd02 key is equal cd02-chave invalid key
                 move 1 to erro
                 perform err-leitura-cd02
           end-start.
      *
       rot-le-cd02-lock.
           move 0 to erro.
           read arqcd02 next. 
           if cd02-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-cd02-lock
           end-if.
           read arqcd02 with kept lock.
      *
       rot-inic-arquivo-c.
           perform lmp-c-contato thru lmp-c-nome.
           move "Inicio do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-fim-arquivo-c.
           perform lmp-c-contato thru lmp-c-nome.
           move "Fim do arquivo - Tecle <Enter>" to mensagem.
           display tela-mensagem.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-display-cont.
           perform rot-move-campos-cont.
           perform dsp-c-contato thru dsp-c-nome.
           if param-prioridade = 9
              move cab-usr to mensagem
              display tela-mensagem
           end-if.
      *
       err-cont-c.
           move " Contato ja cadastrado - Tecle <Enter>" 
           to mensagem.
           display tela-erro-cad-c.
           perform rot-keypress.
           display tela-limpa-cad-c.
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
       sec-acha-contato section.
      *
       rot-acha-cont-00.
           move 0 to erro.
           move c-nome to cd02-chave-1.
           start arqcd02 key is equal cd02-chave-1 
                 invalid key move 1 to erro
           end-start.
           if erro not = 0
              move 1 to erro
              go to rot-acha-cont-fim
           end-if.
      *
       rot-acha-cont-01.
           perform rot-le-cd02-next.
           if erro not = 0 or cd02-chave = high-values
              move 1 to erro
              go to rot-acha-cont-fim
           end-if.
           if cd02-nome-c not = c-nome
               move 1 to erro
              go to rot-acha-cont-fim
           end-if.
           if cd02-codigo = cd01-codigo and 
              cd02-contato not = c-contato-aux
              move 0 to erro
              go to rot-acha-cont-fim
           end-if.
           go to rot-acha-cont-01.
      *
       rot-acha-cont-fim.
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
           accept resposta at 2068 with auto foreground-color 01
                                             background-color 01.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
       accept-resposta-cad-c.
           move spaces to resposta.
           accept resposta at 1468 with auto foreground-color 01
                                             background-color 01.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
      *  Sequencia para dar accept
      *
       acc-codigo.
           accept codigo at 0722 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-nome-fantasia.
           accept nome-fantasia at 0822 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-razao-social.
           accept razao-social at 0922 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-categoria.
           accept categoria at 1022 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-endereco.
           accept endereco at 1222 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-uf.
           accept uf at 1322 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cidade.
           accept cidade at 1338 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cep.
           accept cep at 1364 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-ddd.
           accept ddd at 1422 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-telefone.
           accept telefone at 1454 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-fax.
           accept fax at 1522 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-telex.
           accept telex at 1554 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-endereco-cbr.
           accept endereco-cbr at 1722 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-uf-cbr.
           accept uf-cbr at 1822 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cidade-cbr.
           accept cidade-cbr at 1838 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cep-cbr.
           accept cep-cbr at 1864 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-flag-cgcpf.
           accept flag-cgcpf at 1922 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cgc.
           accept cgc at 1948 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-cpf.
           accept cpf at 1948 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-ie.
           accept ie at 2022 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar Accept (Contatos)
      *
       acc-c-contato.
           accept c-contato at 1220 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
       acc-c-nome.
           accept c-nome at 1320 with auto update prompt
                  foreground-color 15 background-color 01.
           accept escape-key from escape.
           exit.
      *
      *  Sequencia para dar display
      *
       dsp-codigo.
           display codigo at 0722 with foreground-color 15 
                   background-color 01.
      *
       dsp-nome-fantasia.
           display nome-fantasia-a at 0822 with foreground-color 15 
                   background-color 01.
      *
       dsp-razao-social.
           display razao-social-a at 0922 with foreground-color 15 
                   background-color 01.
      *
       dsp-categoria.
           display categoria at 1022 with foreground-color 15 
                   background-color 01.
           display dcategoria at 1026 with foreground-color 15
                   background-color 01.
      *
       dsp-endereco.
           display endereco at 1222 with foreground-color 15 
                   background-color 01.
      *
       dsp-uf.
           display uf at 1322 with foreground-color 15 
                   background-color 01.
      *
       dsp-cidade.
           display cidade at 1338 with foreground-color 15 
                   background-color 01.
      *
       dsp-cep.
           display cep at 1364 with foreground-color 15
                   background-color 01.
      *
       dsp-ddd.
           display ddd at 1422 with foreground-color 15
                   background-color 01.
      *
       dsp-telefone.
           display telefone at 1454 with foreground-color 15 
                   background-color 01.
      *
       dsp-fax.
           display fax at 1522 with foreground-color 15 
                   background-color 01.
      *
       dsp-telex.
           display telex at 1554 with foreground-color 15 
                   background-color 01.
      *
       dsp-endereco-cbr.
           display endereco-cbr at 1722 with foreground-color 15 
                   background-color 01.
      *
       dsp-uf-cbr.
           display uf-cbr at 1822 with foreground-color 15 
                   background-color 01.
      *
       dsp-cidade-cbr.
           display cidade-cbr at 1838 with foreground-color 15 
                   background-color 01.
      *
       dsp-cep-cbr.
           display cep-cbr at 1864 with foreground-color 15 
                   background-color 01.
      *
       dsp-flag-cgcpf.
           display flag-cgcpf at 1922 with foreground-color 15 
                   background-color 01.
      *
       dsp-cgc.
           if flag-cgcpf = "J"
              move cgc to cgc-aux
              display cgc-aux at 1948 with foreground-color 15 
                      background-color 01
           end-if.
      *
       dsp-cpf.
           if flag-cgcpf = "F"
              move cpf to cpf-aux
              display cpf-aux at 1948 with foreground-color 15 
                      background-color 01
           end-if.
      *
       dsp-ie.
           display ie at 2022 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para dar Display (Contatos)
      *
       dsp-c-contato.
           display c-contato at 1220 with foreground-color 15 
                   background-color 01.
      *
       dsp-c-nome.
           display c-nome at 1320 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para fazer limpeza da tela
      *
       lmp-codigo.
           display limpa-45 at 0722 with foreground-color 15 
                   background-color 01.
      *
       lmp-nome-fantasia.
           display limpa-45 at 0822 with foreground-color 15 
                   background-color 01.
      *
       lmp-razao-social.
           display limpa-45 at 0922 with foreground-color 15 
                   background-color 01.
      *
       lmp-categoria.
           display limpa-45 at 1022 with foreground-color 15 
                   background-color 01.
      *
       lmp-endereco.
           display limpa-45 at 1222 with foreground-color 15 
                   background-color 01.
      *
       lmp-uf.
           display "  " at 1322 with foreground-color 15 
                   background-color 01.
      *
       lmp-cidade.
           display limpa-15 at 1338 with foreground-color 15 
                   background-color 01.
      *
       lmp-cep.
           display limpa-08 at 1364 with foreground-color 15
                   background-color 01.
      *
       lmp-ddd.
           display "    " at 1422 with foreground-color 15
                   background-color 01.
      *
       lmp-telefone.
           display limpa-08 at 1454 with foreground-color 15 
                   background-color 01.
      *
       lmp-fax.
           display limpa-08 at 1522 with foreground-color 15 
                   background-color 01.
      *
       lmp-telex.
           display limpa-08 at 1554 with foreground-color 15 
                   background-color 01.
      *
       lmp-endereco-cbr.
           display limpa-45 at 1722 with foreground-color 15 
                   background-color 01.
      *
       lmp-uf-cbr.
           display "  " at 1822 with foreground-color 15 
                   background-color 01.
      *
       lmp-cidade-cbr.
           display limpa-15 at 1838 with foreground-color 15 
                   background-color 01.
      *
       lmp-cep-cbr.
           display limpa-08 at 1864 with foreground-color 15 
                   background-color 01.
      *
       lmp-flag-cgcpf.
           display " " at 1922 with foreground-color 15 
                   background-color 01.
      *
       lmp-cgcpf.
           display "                  " at 1948 with foreground-color 15 
                   background-color 01.
      *
       lmp-ie.
           display limpa-15 at 2022 with foreground-color 15 
                   background-color 01.
      *
      *  Sequencia para dar Display (Contatos)
      *
       lmp-c-contato.
           display limpa-45 at 1220 with foreground-color 15 
                   background-color 01.
      *
       lmp-c-nome.
           display limpa-45 at 1320 with foreground-color 15 
                   background-color 01.
      *
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
       display-erro-usr-c.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad-c.
           perform rot-keypress.
      *
       sec-inclusao section.
      *
       lab-inc-00-0.
           display tela-limpa-cad.
           perform rot-open-cd01.
           if erro not = 0
              go to lab-inc-fim
           end-if.
           perform rot-open-tabl.
           if erro not = 0
              go to lab-inc-fim
           end-if.
      *
       lab-inc-00.
           if param-prioridade < 1
              perform sec-consulta
              go to lab-inc-fim
           end-if.
           display tela-09.
           display tela-02.
      *
       lab-inc-01.
           move spaces to nome-fantasia.
           perform lmp-nome-fantasia.
           perform acc-nome-fantasia.
           if escape-key = 1
              go to lab-inc-fim
           end-if.
           if escape-key = 3
              perform lmp-nome-fantasia
              perform sec-consulta
              display tela-09
              display tela-02
              go to lab-inc-01
           end-if.
           move nome-fantasia to txt nome-fantasia-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-01
           end-if.
           move txt to nome-fantasia cd01-chave-1.
           move 0 to erro.
           start arqcd01 key is equal cd01-chave-1 invalid key
                 move 1 to erro.
           if erro = 0
              perform rot-nome-cad
              if resposta = "N"
                 display tela-02
                 go to lab-inc-01
              end-if
           end-if.
           display tela-limpa-cad.
      *
       lab-inc-02.
           move spaces to razao-social.
           perform lmp-razao-social.
           perform acc-razao-social.
           if escape-key = 1
              perform lmp-razao-social
              display tela-02
              go to lab-inc-01
           end-if.
           move razao-social to txt razao-social-a.
           perform rot-texto.
           if txt = spaces
              move nome-fantasia-a to razao-social-a
              move nome-fantasia to txt
              perform dsp-razao-social
           end-if.
           move txt to razao-social.
           display tela-05.
           move 0 to rotina-codigo.
      *
       lab-inc-03.
           move rotina-codigo to categoria.
           perform lmp-categoria.
           perform acc-categoria.
           if escape-key = 1
              perform lmp-categoria
              display tela-limpa-cad
              go to lab-inc-02
           end-if.
           if escape-key = 3
              move 1 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-inc-03
           end-if.
           if categoria = 0
              go to lab-inc-03
           end-if.
           move 01 to wtab01-tipo.
           move categoria to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-categoria
              go to lab-inc-03
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to dcategoria.
           perform dsp-categoria.
           display tela-limpa-cad.
      *
       lab-inc-04.
           move spaces to endereco.
           perform lmp-endereco.
           perform acc-endereco.
           if escape-key = 1
              perform lmp-endereco
              display tela-05
              go to lab-inc-03
           end-if.
           if endereco = spaces
              go to lab-inc-04
           end-if.
           move spaces to rotina-uf.
           display tela-07.
      *
       lab-inc-05.
           move rotina-uf to uf.
           perform lmp-uf.
           perform acc-uf.
           if escape-key = 1
              perform lmp-uf
              display tela-limpa-cad
              go to lab-inc-04
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-inc-05
           end-if.           
           move uf to txt.
           perform rot-texto.
           move txt to uf.
           if uf = spaces
              go to lab-inc-05
           end-if.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move uf to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-estado
              display tela-07
              go to lab-inc-05
           end-if.
           move reg-tabl to reg-wtab03.
           display tela-limpa-cad.
      *
       lab-inc-06.
           move wtab03-capital to cidade.
           perform lmp-cidade.
           perform acc-cidade.
           if escape-key = 1
              perform lmp-cidade
              display tela-07
              go to lab-inc-05
           end-if.
           if cidade = spaces
              go to lab-inc-06
           end-if.
           move cidade to wtab03-capital.
      *
       lab-inc-07.
           move 0 to cep.
           perform lmp-cep.
           perform acc-cep.
           if escape-key = 1
              perform lmp-cep
              go to lab-inc-06
           end-if.
           if cep = 0
              go to lab-inc-07
           end-if.
      *
       lab-inc-08.
           move wtab03-ddd to ddd.
           perform lmp-ddd.
           perform acc-ddd.
           if escape-key = 1
              perform lmp-ddd
              go to lab-inc-07
           end-if.
           move ddd to wtab03-ddd.
      *
       lab-inc-09.
           move spaces to telefone.
           perform lmp-telefone.
           perform acc-telefone.
           if escape-key = 1
              perform lmp-telefone
              go to lab-inc-08
           end-if.
      *
       lab-inc-10.
           move spaces to fax.
           perform lmp-fax.
           perform acc-fax.
           if escape-key = 1
              perform lmp-fax
              go to lab-inc-09
           end-if.
      *
       lab-inc-11.
           move spaces to telex.
           perform lmp-telex.
           perform acc-telex.
           if escape-key = 1
              perform lmp-telex
              go to lab-inc-10
           end-if.
      *
       lab-inc-12.
           move spaces to endereco-cbr.
           perform lmp-endereco-cbr.
           perform acc-endereco-cbr.
           if escape-key = 1
              perform lmp-endereco-cbr
              go to lab-inc-11
           end-if.
           if endereco-cbr = spaces
              move endereco to endereco-cbr
              perform dsp-endereco-cbr
           end-if.
           move spaces to rotina-uf.
           display tela-07.
      *
       lab-inc-13.
           move rotina-uf to uf-cbr.
           perform lmp-uf-cbr.
           perform acc-uf-cbr.
           if escape-key = 1
              perform lmp-uf-cbr
              display tela-limpa-cad
              go to lab-inc-12
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-inc-13
           end-if.           
           move uf-cbr to txt.
           perform rot-texto.
           move txt to uf-cbr.
           if uf-cbr = spaces
              move uf to uf-cbr
           end-if.
           perform dsp-uf-cbr.
           move 03 to wtab03-tipo.
           move uf-cbr to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-estado
              display tela-07
              go to lab-inc-13
           end-if.
           move reg-tabl to reg-wtab03.
           display tela-limpa-cad.
      *
       lab-inc-14.
           move wtab03-capital to cidade-cbr.
           perform lmp-cidade-cbr.
           perform acc-cidade-cbr.
           if escape-key = 1
              perform lmp-cidade-cbr
              display tela-07
              go to lab-inc-13
           end-if.
           if cidade-cbr = spaces
              move cidade to cidade-cbr
           perform dsp-cidade-cbr
           end-if.
           move cidade-cbr to wtab03-capital.
      *
       lab-inc-15.
           move 0 to cep-cbr.
           perform lmp-cep-cbr.
           perform acc-cep-cbr.
           if escape-key = 1
              perform lmp-cep-cbr
              go to lab-inc-14
           end-if.
           if cep-cbr = 0
              move cep to cep-cbr
              perform dsp-cep-cbr
           end-if.
           display tela-08.
      *
       lab-inc-16.
           move spaces to flag-cgcpf.
           perform lmp-flag-cgcpf.
           perform acc-flag-cgcpf.
           if escape-key = 1
              perform lmp-flag-cgcpf
              display tela-limpa-cad
              go to lab-inc-15
           end-if.
           move flag-cgcpf to txt.
           perform rot-texto.
           move txt to flag-cgcpf.
           if txt not = "F" and "J"
              go to lab-inc-16
           end-if.
           perform dsp-flag-cgcpf
           display tela-limpa-cad.
      *
       lab-inc-17.
           move 0 to cpf cgc.
           perform lmp-cgcpf.
           if flag-cgcpf = "J"
              perform acc-cgc
           else
              perform acc-cpf
           end-if.
           if escape-key = 1
              perform lmp-cgcpf
              go to lab-inc-16
           end-if.
           if cgc = 0 and cpf
              go to lab-inc-17
           end-if.
           if flag-cgcpf = "J"
              move cgc to cgc-aux
              perform dsp-cgc
           else
              move cpf to cpf-aux
              perform dsp-cpf
           end-if.
      *
       lab-inc-18.
           move 0 to ie.
           perform lmp-ie.
           perform acc-ie.
           if escape-key = 1
              perform lmp-ie
              go to lab-inc-17
           end-if.
           if ie = 0
              perform lmp-ie
           end-if.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-inc-19.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-inc-18
           end-if.
           if resposta = "N"
              perform lmp-codigo thru lmp-ie
              display tela-02
              go to lab-inc-01
           else
              if resposta not = "S"
                 go to lab-inc-19
              end-if
           end-if.
           move high-values to cd01-chave-controle.
      *
       lab-inc-20.
           perform rot-ponteiro.
           if erro not = 0
              move " Erro no registro de controle - ARQCD01A.DAT - Tecle
      -       " <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              go to lab-inc-fim
           end-if.
           perform rot-le-cd01-lock.
           add 1 to cd01-numero.
           move cd01-numero to codigo.
           rewrite reg-cd01.
           unlock arqcd01 record.
           perform rot-move-cd01.
           write reg-cd01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQCD01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-fim
           end-write.
           perform dsp-codigo.
           display tela-06
           perform sec-contato.
           perform rot-keypress
           perform lmp-codigo thru lmp-ie.
           display tela-02.
           go to lab-inc-01.
      *
       lab-inc-fim.
           perform rot-close-cd01.
           perform rot-close-tabl.
           exit.
      *
       sec-consulta section.
      *
       lab-cns-00.
           display tela-limpa-cad.
           display tela-10.
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
                            when kbd2 = 78 or 110
                                 display tela-limpa-cad
                                 perform sec-consulta-nome-fantasia
                                 display tela-03
                            when kbd2 = 82 or 114
                                 display tela-limpa-cad
                                 perform sec-consulta-razao-social
                                 display tela-03
                             when kbd2 = 65 or 97
                                 display tela-limpa-cad
                                 perform sec-consulta-categoria
                                 display tela-03
                             when kbd2 = 71 or 103
                                 display tela-08
                                 perform sec-consulta-cgcpf
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
           display tela-04.
           move low-values to cd01-chave.
           move codigo to cd01-codigo.
      *
       lab-cns-codigo-00-a.
           start arqcd01 key is not less cd01-chave.
           go to lab-cns-codigo-03.
      *
       lab-cns-codigo-01.
           perform rot-le-anterior.
           if erro not = 0 or cd01-codigo = codigo
              perform rot-inic-arquivo
              start arqcd01 key is not less cd01-chave
              move 1 to erro
              go to lab-cns-codigo-05
           end-if.
           if cd01-chave = high-values
              go to lab-cns-codigo-01
           end-if.
           go to lab-cns-codigo-04.
      *
       lab-cns-codigo-02.
           start arqcd01 key is less cd01-chave.
      *
       lab-cns-codigo-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-cd01
              go to lab-cns-codigo-fim
           end-if.
           if cd01-chave = high-values
              perform rot-fim-arquivo
              start arqcd01 key is not less cd01-chave
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
                            perform sec-contato
                            go to lab-cns-codigo-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-codigo-03
                    when kbd-aux = 73
                         go to lab-cns-codigo-01
                    when kbd-aux = 71
                         move low-values to cd01-chave
                         go to lab-cns-codigo-00-a
                    when kbd-aux = 79
                         move high-values to cd01-chave
                         go to lab-cns-codigo-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-codigo-05
           end-if.
           perform lmp-codigo thru lmp-ie.
           display tela-limpa-cad.
           go to lab-cns-codigo-00.
      *
       lab-cns-codigo-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-ie.
           exit.
      *
       sec-consulta-nome-fantasia section.
      *
       lab-cns-nome-fantasia-00.
           move spaces to nome-fantasia.
           perform lmp-nome-fantasia.
           perform acc-nome-fantasia.
           if escape-key = 1
              perform lmp-nome-fantasia
              go to lab-cns-nome-fantasia-fim
           end-if.
           display tela-04.
           move nome-fantasia to txt.
           perform rot-texto.
           move low-values to cd01-chave-1.
           move txt to cd01-nome-fantasia in cd01-chave-1.
      *
       lab-cns-nome-fantasia-00-a.
           start arqcd01 key is not less cd01-chave-1.
           go to lab-cns-nome-fantasia-03.
      *
       lab-cns-nome-fantasia-01.
           perform rot-le-anterior.
           if erro not = 0 or cd01-codigo = codigo
              perform rot-inic-arquivo
              start arqcd01 key is not less cd01-chave-1
              move 1 to erro
              go to lab-cns-nome-fantasia-05
           end-if.
           if cd01-chave = high-values
              go to lab-cns-nome-fantasia-01
           end-if.
           go to lab-cns-nome-fantasia-04.
      *
       lab-cns-nome-fantasia-02.
           start arqcd01 key is less cd01-chave-1.
      *
       lab-cns-nome-fantasia-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-cd01
              go to lab-cns-nome-fantasia-fim
           end-if.
           if cd01-chave = high-values
              perform rot-fim-arquivo
              start arqcd01 key is not less cd01-chave-1
              move 0 to codigo
              move 1 to erro
              go to lab-cns-nome-fantasia-05
           end-if.
      *
       lab-cns-nome-fantasia-04.
           perform rot-display.
      *
       lab-cns-nome-fantasia-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-nome-fantasia-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-nome-fantasia-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform sec-contato
                            go to lab-cns-nome-fantasia-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-nome-fantasia-03
                    when kbd-aux = 73
                         go to lab-cns-nome-fantasia-01
                    when kbd-aux = 71
                         move low-values to cd01-chave-1
                         go to lab-cns-nome-fantasia-00-a
                    when kbd-aux = 79
                         move high-values to cd01-chave-1
                         go to lab-cns-nome-fantasia-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-nome-fantasia-05
           end-if.
           perform lmp-codigo thru lmp-ie.
           display tela-limpa-cad.
           go to lab-cns-nome-fantasia-00.
      *
       lab-cns-nome-fantasia-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-ie.
           exit.
      *
       sec-consulta-razao-social section.
      *
       lab-cns-razao-social-00.
           move spaces to razao-social.
           perform lmp-razao-social.
           perform acc-razao-social.
           if escape-key = 1
              perform lmp-razao-social
              go to lab-cns-razao-social-fim
           end-if.
           display tela-04.
           move razao-social to txt.
           perform rot-texto.
           move low-values to cd01-chave-2.
           move txt to cd01-razao-social in cd01-chave-2.
      *
       lab-cns-razao-social-00-a.
           start arqcd01 key is not less cd01-chave-2.
           go to lab-cns-razao-social-03.
      *
       lab-cns-razao-social-01.
           perform rot-le-anterior.
           if erro not = 0 or cd01-codigo = codigo
              perform rot-inic-arquivo
              start arqcd01 key is not less cd01-chave-2
              move 1 to erro
              go to lab-cns-razao-social-05
           end-if.
           if cd01-chave = high-values
              go to lab-cns-razao-social-01
           end-if.
           go to lab-cns-razao-social-04.
      *
       lab-cns-razao-social-02.
           start arqcd01 key is less cd01-chave-2.
      *
       lab-cns-razao-social-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-cd01
              go to lab-cns-razao-social-fim
           end-if.
           if cd01-chave = high-values
              perform rot-fim-arquivo
              start arqcd01 key is not less cd01-chave-2
              move 0 to codigo
              move 1 to erro   
           go to lab-cns-razao-social-05
           end-if.
      *
       lab-cns-razao-social-04.
           perform rot-display.
      *
       lab-cns-razao-social-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-razao-social-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-razao-social-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform sec-contato
                            go to lab-cns-razao-social-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-razao-social-03
                    when kbd-aux = 73
                         go to lab-cns-razao-social-01
                    when kbd-aux = 71
                         move low-values to cd01-chave-2
                         go to lab-cns-razao-social-00-a
                    when kbd-aux = 79
                         move high-values to cd01-chave-2
                         go to lab-cns-razao-social-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-razao-social-05
           end-if.
           perform lmp-codigo thru lmp-ie.
           display tela-limpa-cad.
           go to lab-cns-razao-social-00.
      *
       lab-cns-razao-social-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-ie.
           exit.
      *
       sec-consulta-categoria section.
      *
       lab-cns-categoria-00.
           move 0 to categoria.
           perform lmp-categoria.
           perform acc-categoria.
           if escape-key = 1
              perform lmp-categoria
              go to lab-cns-categoria-fim
           end-if.
           display tela-04.
           move low-values to cd01-chave-3.
           move categoria to cd01-categoria.
      *
       lab-cns-categoria-00-a.
           start arqcd01 key is not less cd01-chave-3.
           go to lab-cns-categoria-03.
      *
       lab-cns-categoria-01.
           perform rot-le-anterior.
           if erro not = 0 or cd01-codigo = codigo
              perform rot-inic-arquivo
              start arqcd01 key is not less cd01-chave-3
              move 1 to erro
              go to lab-cns-categoria-05
           end-if.
           if cd01-chave = high-values
              go to lab-cns-categoria-01
           end-if.
           go to lab-cns-categoria-04.
      *
       lab-cns-categoria-02.
           start arqcd01 key is less cd01-chave-3.
      *
       lab-cns-categoria-03.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-cd01
              go to lab-cns-categoria-fim
           end-if.
           if cd01-chave = high-values
              perform rot-fim-arquivo
              start arqcd01 key is not less cd01-chave-3
              move 0 to codigo
              move 1 to erro
              go to lab-cns-categoria-05
           end-if.
      *
       lab-cns-categoria-04.
           perform rot-display.
      *
       lab-cns-categoria-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-categoria-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-categoria-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform sec-contato
                            go to lab-cns-categoria-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-categoria-03
                    when kbd-aux = 73
                         go to lab-cns-categoria-01
                    when kbd-aux = 71
                         move 0 to cd01-chave-3
                         go to lab-cns-categoria-00-a
                    when kbd-aux = 79
                         move 99 to cd01-chave-3
                         go to lab-cns-categoria-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-categoria-05
           end-if.
           perform lmp-codigo thru lmp-ie.
           display tela-limpa-cad.
           go to lab-cns-categoria-00.
      *
       lab-cns-categoria-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-ie.
           exit.
      *  
       sec-consulta-cgcpf section.
      *
       lab-cns-cgcpf-00.
           move spaces to flag-cgcpf.
           perform lmp-flag-cgcpf.
           perform acc-flag-cgcpf.
           if escape-key = 1
              perform lmp-flag-cgcpf
              display tela-limpa-cad
              go to lab-cns-cgcpf-fim
           end-if.
           move flag-cgcpf to txt.
           perform rot-texto.
           move txt to flag-cgcpf.
           if txt not = "F" and "J"
              go to lab-cns-cgcpf-00
           end-if.
           perform dsp-flag-cgcpf
           display tela-limpa-cad.
       lab-cns-cgcpf-00-0.
           move 0 to cpf cgc.
           perform lmp-cgcpf.
           if flag-cgcpf = "J"
              perform acc-cgc
           else
              perform acc-cpf
           end-if.
           if escape-key = 1
              perform lmp-cgcpf
              display tela-08
              go to lab-cns-cgcpf-00
           end-if.
           move low-values to cd01-chave-4.
           if flag-cgcpf = "J"
              move cgc to cgc-aux cd01-cgcpf
              perform dsp-cgc
           else
              move cpf to cpf-aux cd01-cgcpf
              perform dsp-cpf
           end-if.
           move flag-cgcpf to cd01-flag-cgcpf.
      *
       lab-cns-cgcpf-00-a.
           start arqcd01 key is not less cd01-chave-4.
           go to lab-cns-cgcpf-03.
      *
       lab-cns-cgcpf-01.
           perform rot-le-anterior.
           if erro not = 0 or cd01-codigo = codigo
              perform rot-inic-arquivo
              start arqcd01 key is not less cd01-chave-4
              move 1 to erro
              go to lab-cns-cgcpf-05
           end-if.
           if cd01-chave = high-values
              go to lab-cns-cgcpf-01
           end-if.
           go to lab-cns-cgcpf-04.
      *
       lab-cns-cgcpf-02.
           start arqcd01 key is less cd01-chave-4.
      *
       lab-cns-cgcpf-03.
           perform rot-le-proximo.
           if erro not = 0
              perform rot-erro-leitura-cd01
              go to lab-cns-cgcpf-fim
           end-if.
           if cd01-chave = high-values
              perform rot-fim-arquivo
              start arqcd01 key is not less cd01-chave-4
              move 0 to codigo
              move 1 to erro
              go to lab-cns-cgcpf-05
           end-if.
      *
       lab-cns-cgcpf-04.
           perform rot-display.
      *
       lab-cns-cgcpf-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao
                            go to lab-cns-cgcpf-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao
                            go to lab-cns-cgcpf-00-a
                         end-if
                    when kbd-aux = 62
                         if erro = 0
                            perform sec-contato
                            go to lab-cns-cgcpf-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-cgcpf-03
                    when kbd-aux = 73
                         go to lab-cns-cgcpf-01
                    when kbd-aux = 71
                         move low-values to cd01-chave-4
                         go to lab-cns-cgcpf-00-a
                    when kbd-aux = 79
                         move high-values to cd01-chave-4
                         go to lab-cns-cgcpf-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-cgcpf-05
           end-if.
           perform lmp-codigo thru lmp-ie.
           display tela-08.
           go to lab-cns-cgcpf-00.
      *
       lab-cns-cgcpf-fim.
           move zeros to campo-kbd.
           perform lmp-codigo thru lmp-ie.
           exit.
      *
       sec-exclusao section.
      *
       lab-exc-00-0.
           if codigo = 1
              move " Registro nao pode ser excluido - Tecle <Enter>"
              to mensagem
              display tela-erro-cad
              perform rot-keypress
              display tela-limpa-cad
              go to lab-exc-fim
           end-if.
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
           perform rot-le-cd01-lock.
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
           delete arqcd01 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQCD01A.DAT - Tecle <Enter>
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
           unlock arqcd01 record.
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
           perform rot-le-cd01-lock.
           perform rot-display.
      *
       lab-alt-01.
           perform acc-nome-fantasia.
           if escape-key = 1
              go to lab-alt-fim
           end-if.
           move nome-fantasia to txt nome-fantasia-a.
           perform rot-texto.
           if txt = spaces
              go to lab-alt-01
           end-if.
           move txt to nome-fantasia cd01-chave-1.
           move 0 to erro.
           start arqcd01 key is equal cd01-chave-1 invalid key 
                 display tela-limpa-cad
                 go to lab-alt-02
           end-start.
           perform rot-le-proximo.
           if cd01-codigo not = codigo
              perform rot-nome-cad
              move nome-fantasia-a to razao-social-a
              if resposta = "N"
                 go to lab-alt-01
              end-if
           end-if.
           display tela-limpa-cad.
      *
       lab-alt-02.
           perform acc-razao-social.
           if escape-key = 1
              move nome-fantasia-a to nome-fantasia
              go to lab-alt-01
           end-if.
           move razao-social to txt razao-social-a.
           perform rot-texto.
           if txt = spaces
              move nome-fantasia-a to razao-social-a
              move nome-fantasia to razao-social
              perform dsp-razao-social
           end-if.
           move txt to razao-social.
           display tela-05.
           move categoria to rotina-codigo.
      *
       lab-alt-03.
           move rotina-codigo to categoria.
           perform lmp-categoria.
           perform acc-categoria.
           if escape-key = 1
              perform dsp-categoria
              display tela-limpa-cad
              move razao-social-a to razao-social
              go to lab-alt-02
           end-if.
           if escape-key = 3
              move 1 to rotina-tipo
              perform rot-pesq-tabela
              go to lab-alt-03
           end-if.
           if categoria = 0
              go to lab-alt-03
           end-if.
           move 01 to wtab01-tipo.
           move categoria to wtab01-codigo rotina-codigo.
           move spaces to wtab01-resto.
           move wtab01-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-categoria
              go to lab-alt-03
           end-if.
           move reg-tabl to reg-wtab01.
           move wtab01-descricao to dcategoria
           perform dsp-categoria.
           display tela-limpa-cad.
      *
       lab-alt-04.
           perform acc-endereco.
           if escape-key = 1
              display tela-05
              go to lab-alt-03
           end-if.
           if endereco = spaces
              go to lab-alt-04
           end-if.
           move uf to rotina-uf.
           display tela-07.
      *
       lab-alt-05.
           move rotina-uf to uf.
           perform acc-uf.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-04
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-alt-05
           end-if.           
           move uf to txt.
           perform rot-texto.
           move txt to uf.
           if uf = spaces
              go to lab-alt-05
           end-if.
           perform dsp-uf.
           move 03 to wtab03-tipo.
           move uf to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-estado
              display tela-07
              go to lab-alt-05
           end-if.
           display tela-limpa-cad.
      *
       lab-alt-06.
           perform acc-cidade.
           if escape-key = 1
              display tela-07
              go to lab-alt-05
           end-if.
           if cidade = spaces
              go to lab-alt-06
           end-if.
      *
       lab-alt-07.
           perform acc-cep.
           if escape-key = 1
              go to lab-alt-06
           end-if.
           if cep = 0
              go to lab-alt-07
           end-if.
      *
       lab-alt-08.
           perform acc-ddd.
           if escape-key = 1
              go to lab-alt-07
           end-if.
      *
       lab-alt-09.
           perform acc-telefone.
           if escape-key = 1
              go to lab-alt-08
           end-if.
      *
       lab-alt-10.
           perform acc-fax.
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
           perform acc-endereco-cbr.
           if escape-key = 1
              go to lab-alt-11
           end-if.
           if endereco-cbr = spaces
              move endereco to endereco-cbr
              perform dsp-endereco-cbr
           end-if.
           move uf-cbr to rotina-uf.
           display tela-07.
      *
       lab-alt-13.
           move rotina-uf to uf-cbr.
           perform acc-uf-cbr.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-12
           end-if.
           if escape-key = 3
              move 3 to rotina-tipo-uf
              perform rot-pesq-tabela-3
              go to lab-alt-13
           end-if.           
           move uf-cbr to txt.
           perform rot-texto.
           move txt to uf-cbr.
           if uf-cbr = spaces
              move uf to uf-cbr
           end-if.
           perform dsp-uf-cbr.
           move 03 to wtab03-tipo.
           move uf-cbr to wtab03-sigla rotina-uf.
           move spaces to wtab03-resto.
           move wtab03-chave to tabl-chave.
           perform rot-le-tabl.
           if erro not = 0
              perform rot-erro-estado
              display tela-07
              go to lab-alt-13
           end-if.
           display tela-limpa-cad.
      *
       lab-alt-14.
           perform acc-cidade-cbr.
           if escape-key = 1
              display tela-07
              go to lab-alt-13
           end-if.
           if cidade-cbr = spaces
              move cidade to cidade-cbr
              perform dsp-cidade-cbr
           end-if.
      *
       lab-alt-15.
           perform acc-cep-cbr.
           if escape-key = 1
              go to lab-alt-14
           end-if.
           if cep-cbr = 0
              move cep to cep-cbr
              perform dsp-cep-cbr
           end-if.
           display tela-08.
      *
       lab-alt-16.
           perform acc-flag-cgcpf.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-15
           end-if.
           move flag-cgcpf to txt.
           perform rot-texto.
           move txt to flag-cgcpf.
           if txt not = "F" and "J"
              go to lab-alt-16
           end-if.
           perform dsp-flag-cgcpf
           display tela-limpa-cad.
      *
       lab-alt-17.
           perform lmp-cgcpf.
           if flag-cgcpf = "J"
              perform acc-cgc
           else
              perform acc-cpf
           end-if.
           if escape-key = 1
              go to lab-alt-16
           end-if.
           if cgc = 0 and cpf
              go to lab-alt-17
           end-if.
           if flag-cgcpf = "J"
              move cgc to cgc-aux
              perform dsp-cgc
           else
              move cpf to cpf-aux
              perform dsp-cpf
           end-if.
      *
       lab-alt-18.
           perform acc-ie.
           if escape-key = 1
              go to lab-alt-17
           end-if.
           if ie = 0
              perform lmp-ie
           end-if.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad.
      *
       lab-alt-19.
           perform accept-resposta-cad.
           if escape-key = 1
              display tela-limpa-cad
              go to lab-alt-18
           end-if.
           if resposta = "N"
              go to lab-alt-fim
           else
              if resposta not = "S"
                 go to lab-alt-19
              end-if
           end-if.
           perform rot-move-cd01.
           rewrite reg-cd01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQCD01A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
                   go to lab-alt-fim
           end-rewrite.
           move "Registro alterado - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad.
           perform rot-keypress.
           display tela-limpa-cad.
      *
       lab-alt-fim.
           unlock arqcd01 record.
           display tela-04.
           exit.
      *
       sec-contato section.
      *
       lab-cont-00.
           move 0 to box-col box-lin.
           move 79 to box-col-f.
           move 24 to box-lin-f.
           perform rot-save-buffer-01.
           move 06 to box-col.
           move 09 to box-lin.
           move 70 to box-col-f.
           move 15 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           move spaces to box-fundo.
           move "S" to box-sombra.
           perform rot-box.
           display tela-12.
      *
       lab-cont-01.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-cont-fim
           end-if.
           perform rot-le-cd01-lock.
           perform sec-inclusao-cont.
           unlock arqcd01 record.
           move 0 to box-col box-lin.
           move 79 to box-col-f.
           move 24 to box-lin-f.
           perform rot-rest-buffer-01.
      *
       lab-cont-fim.
           exit.
      *
       sec-inclusao-cont section.
      *
       lab-inc-cont-00.
           display tela-limpa-cad-c.
           perform rot-open-cd02.
           if erro not = 0
              go to lab-inc-cont-fim
           end-if.
           if param-prioridade < 1
              perform sec-consulta-cont
              go to lab-inc-cont-fim
           end-if.
           display tela-13.
      *
       lab-inc-cont-01.
           display tela-15.
           move 0 to c-contato.
           perform lmp-c-contato.
           perform acc-c-contato.
           if escape-key = 1 
              go to lab-inc-cont-fim
           end-if.
           if escape-key = 3
              perform lmp-c-contato
              perform sec-consulta-cont
              display tela-13
              go to lab-inc-cont-01
           end-if.
           if c-contato = 0
              go to lab-inc-cont-01
           end-if.
           move cd01-codigo to cd02-codigo.
           move c-contato to cd02-contato.
           perform rot-le-cd02.
           if erro = 0
              perform err-cont-c
              go to lab-inc-cont-01
           end-if.
           display tela-limpa-cad-c.
      *
       lab-inc-cont-02.
           move spaces to c-nome.
           perform lmp-c-nome.
           perform acc-c-nome.
           if escape-key = 1
              perform lmp-c-nome
              go to lab-inc-cont-01
           end-if. 
           move c-nome to txt c-nome-a.
           perform rot-texto.
           if txt = spaces
              go to lab-inc-cont-02
           end-if.
           move txt to cd02-nome-c c-nome.
           move 0 to c-contato-aux.
           perform sec-acha-contato.
           if erro = 0
              perform err-cont-c
              go to lab-inc-cont-02
           end-if.
           move "Cadastrar (S) (N) ?" to mensagem.
           display tela-mensagem-cad-c.
      *
       lab-inc-cont-03.
           perform accept-resposta-cad-c.
           if escape-key = 1
              display tela-limpa-cad-c
              go to lab-inc-cont-02
           end-if.
           if resposta = "N"
              perform lmp-c-contato thru lmp-c-nome
              go to lab-inc-cont-01
           else
              if resposta not = "S"
                 go to lab-inc-cont-03
              end-if
           end-if.
      *
       lab-inc-cont-16.
           perform rot-move-cd02.
           write reg-cd02 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQCD02A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-inc-cont-fim
           end-write.
           display tela-17.
           perform rot-keypress.
           perform lmp-c-contato thru lmp-c-nome.
           go to lab-inc-cont-01.
      *
       lab-inc-cont-fim.
           perform rot-close-cd02.
           exit.
      *
       sec-consulta-cont section.
      *
       lab-cns-cont-00.
           display tela-limpa-cad-c.
           display tela-14.
      *
       lab-cns-cont-01.
           display tela-18.
           move 0 to kbd2.
           perform until kbd2 = 27
                   perform rot-keypress
                   evaluate true
                            when kbd2 = 67 or 99
                                 display tela-limpa-cad-c
                                 perform sec-consulta-contato
                                 display tela-18
                            when kbd2 = 78 or 110
                                 display tela-limpa-cad-c
                                 perform sec-consulta-nome-cont
                                 display tela-18
                   end-evaluate
                   display tela-limpa
           end-perform.
      *
       lab-cns-cont-fim.
           display tela-limpa.
           exit.
      *
       sec-consulta-contato section.
      *
       lab-cns-cont-00.
           move 0 to c-contato.
           perform lmp-c-contato.
           perform acc-c-contato.
           if escape-key = 1
              perform lmp-c-contato
              go to lab-cns-cont-fim
           end-if.
           display tela-19.
           move cd01-codigo to cd02-codigo.
           move c-contato to cd02-contato.
      *
       lab-cns-cont-00-a.
           start arqcd02 key is not less cd02-chave.
           go to lab-cns-cont-03.
      *
       lab-cns-cont-01.
           perform rot-le-cd02-prev.
           if erro not = 0 or 
              cd02-codigo not = cd01-codigo or c-flag = 2
              perform rot-inic-arquivo-c
              move cd01-codigo to cd02-codigo
              move 0 to cd02-contato
              start arqcd02 key is not less cd02-chave
              move 2 to c-flag
              move 1 to erro
              go to lab-cns-cont-05
           end-if.
           if cd02-chave = high-values
              go to lab-cns-cont-01
           end-if.
           go to lab-cns-cont-04.
      *
       lab-cns-cont-02.
           start arqcd02 key is less cd02-chave.
      *
       lab-cns-cont-03.
           perform rot-le-cd02-next.
           if erro not = 0
              perform err-leitura-cd02
              go to lab-cns-cont-fim
           end-if.
           if cd02-chave = high-values or 
              cd02-codigo not = cd01-codigo or c-flag = 1
              move cd01-codigo to cd02-codigo
              move 99 to cd02-contato
              perform rot-fim-arquivo-c
              start arqcd02 key is less cd02-chave
              move 1 to erro
              move 1 to c-flag
              go to lab-cns-cont-05
           end-if.
      *
       lab-cns-cont-04.
           perform rot-display-cont.
           move 0 to c-flag.
      *
       lab-cns-cont-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao-cont
                            go to lab-cns-cont-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao-cont
                            go to lab-cns-cont-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-cont-03
                    when kbd-aux = 73
                         go to lab-cns-cont-01
                    when kbd-aux = 71
                         move cd01-codigo to cd02-codigo
                         move 0 to cd02-contato
                         move 0 to c-flag
                         go to lab-cns-cont-00-a
                    when kbd-aux = 79
                         move cd01-codigo to cd02-codigo
                         move 99 to cd02-contato
                         move 0 to c-flag
                         go to lab-cns-cont-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-cont-05
           end-if.
           perform lmp-c-contato thru lmp-c-nome.
           display tela-limpa-cad-c.
           display tela-limpa.
           go to lab-cns-cont-00.
      *
       lab-cns-cont-fim.
           move zeros to campo-kbd.
           perform lmp-c-contato thru lmp-c-nome.
           display tela-limpa.
           exit.
      *
       sec-consulta-nome-cont section.
      *
       lab-cns-nome-cont-00.
           move spaces to c-nome.
           perform lmp-c-nome.
           perform acc-c-nome.
           if escape-key = 1
              perform lmp-c-nome
              go to lab-cns-nome-cont-fim
           end-if.
           display tela-19.
           move c-nome to txt.
           perform rot-texto.
           move txt to cd02-chave-1.
      *
       lab-cns-nome-cont-00-a.
           start arqcd02 key is not less cd02-chave-1.
           go to lab-cns-nome-cont-03.
      *
       lab-cns-nome-cont-01.
           perform rot-le-cd02-prev.
           if erro not = 0 or 
              cd02-controle = c-controle 
              perform rot-inic-arquivo-c
              start arqcd02 key is not less cd02-chave-1
              move 1 to erro
              go to lab-cns-nome-cont-05
           end-if.
           if cd02-chave = high-values
              go to lab-cns-nome-cont-01
           end-if.
           move cd02-controle to c-controle.
           go to lab-cns-nome-cont-04.
      *
       lab-cns-nome-cont-02.
           start arqcd02 key is less cd02-chave-1.
      *
       lab-cns-nome-cont-03.
           move 0 to erro.
           perform rot-le-cd02-next.
           if erro not = 0
              perform err-leitura-cd02
              go to lab-cns-nome-cont-fim
           end-if.
           if cd02-chave = high-values
              perform rot-fim-arquivo-c
              start arqcd02 key is not less cd02-chave-1
              move 1 to erro
              go to lab-cns-nome-cont-05
           end-if.
      *
       lab-cns-nome-cont-04.
           unlock arqcd01 record.
           move cd02-codigo to cd01-codigo.
           perform rot-ponteiro.
           if erro not = 0
              go to lab-cns-nome-cont-fim
           end-if.
           perform rot-le-cd01-lock.
           perform rot-display-cont.
      *
       lab-cns-nome-cont-05.
           perform rot-keypress.
           move kbd1 to kbd-aux.
           evaluate true
                    when kbd-aux = 60  
                         if erro = 0
                            perform sec-alteracao-cont
                            go to lab-cns-nome-cont-00-a
                         end-if
                    when kbd-aux = 61 
                         if erro = 0
                            perform sec-exclusao-cont
                            go to lab-cns-nome-cont-00-a
                         end-if
                    when kbd-aux = 81 
                         go to lab-cns-nome-cont-03
                    when kbd-aux = 73
                         go to lab-cns-nome-cont-01
                    when kbd-aux = 71
                         move low-values to cd02-chave-1
                         go to lab-cns-nome-cont-00-a
                    when kbd-aux = 79
                         move high-values to cd02-chave-1
                         go to lab-cns-nome-cont-02
           end-evaluate.
           if kbd-aux not = 1
              go to lab-cns-nome-cont-05
           end-if.
           perform lmp-c-contato thru lmp-c-nome.
           display tela-limpa-cad-c.
           display tela-limpa.
           go to lab-cns-nome-cont-00.
      *
       lab-cns-nome-cont-fim.
           move zeros to campo-kbd.
           perform lmp-c-contato thru lmp-c-nome.
           display tela-limpa.
           exit.
      *
       sec-exclusao-cont section.
      *
       lab-exc-cont-00-0.
           display tela-limpa-cad-c.
           if param-prioridade < 7
              perform display-erro-usr-c
              go to lab-exc-cont-fim
           end-if.
           perform rot-ponteiro-cd02.
           if erro not = 0
              go to lab-exc-cont-fim
           end-if.
      *
       lab-exc-cont-00.
           perform rot-le-cd02-lock.
           perform rot-display-cont.
           move "Excluir (S) (N) ?" to mensagem.
           display tela-mensagem-cad-c.
      *
       lab-exc-cont-01.
           perform accept-resposta-cad-c.
           if escape-key = 1
              display tela-limpa-cad-c
              go to lab-exc-cont-fim
           end-if.
           if resposta = "N"
              go to lab-exc-cont-fim
           else
              if resposta not = "S"
                 go to lab-exc-cont-01
              end-if
           end-if.
           delete arqcd02 invalid key 
                  move 1 to erro
                  move " Erro de exclusao - ARQCD02A.DAT - Tecle <Enter>
      -           " " to mensagem
                  display tela-erro
                  perform rot-keypress
                  display tela-limpa
                  go to lab-exc-cont-fim
           end-delete.
           move "Registro excluido - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad-c.
           perform rot-keypress.
           display tela-limpa-cad-c.
      *
       lab-exc-cont-fim.
           unlock arqcd02 record.
           display tela-19.
           exit.
      *
       sec-alteracao-cont section.
      *
       lab-alt-00-0.
           display tela-limpa-cad-c.
           if param-prioridade < 5
              perform display-erro-usr-c
              go to lab-alt-cont-fim
           end-if.
           perform rot-ponteiro-cd02.
           if erro not = 0
              go to lab-alt-cont-fim
           end-if.
           perform rot-le-cd02-lock.
           perform rot-display-cont.
           move c-contato to c-contato-aux.
      *
       lab-alt-cont-01.
           perform lmp-c-contato.
           perform acc-c-contato.
           if escape-key = 1
              go to lab-alt-cont-fim
           end-if.
           if c-contato = 0
              go to lab-alt-cont-01
           end-if.
           if c-contato not equal c-contato-aux
              move cd01-codigo to cd02-codigo
              move c-contato to cd02-contato
              perform rot-le-cd02
              if erro = 0
                 perform err-cont-c
                 go to lab-alt-cont-01
              end-if
           end-if.
      *
       lab-alt-cont-02.
           perform acc-c-nome.
           if escape-key = 1
              go to lab-alt-cont-01
           end-if.
           move c-nome to txt c-nome-a.
                 perform rot-texto.
           if txt = spaces
              go to lab-alt-cont-02
           end-if.
           move txt to cd02-nome-c c-nome.
           perform sec-acha-contato.
           if erro = 0 
              perform err-cont-c
              go to lab-alt-cont-02
           end-if.
           move "Alterar (S) (N) ?" to mensagem.
           display tela-mensagem-cad-c.
      *
       lab-alt-cont-03.
           perform accept-resposta-cad-c.
           if escape-key = 1
              move c-nome-a to c-nome
              display tela-limpa-cad-c
              go to lab-alt-cont-02
           end-if.
           if resposta = "N"
              go to lab-alt-cont-fim
           else
              if resposta not = "S"
                 go to lab-alt-cont-03
              end-if
           end-if.
      *
       lab-alt-cont-04.
           move cd01-codigo to cd02-codigo.
           move c-contato-aux to cd02-contato.
           perform rot-ponteiro-cd02.
           if erro not = 0
              go to lab-alt-cont-fim
           end-if.
           perform rot-le-cd02-lock.
           if c-contato not = c-contato-aux
              delete arqcd02 invalid key 
                     move 1 to erro
                     move " Erro de exclusao - ARQCD02A.DAT - Tecle <Ent
      -              "er>" to mensagem
                     display tela-erro
                     perform rot-keypress
                     display tela-limpa
                     go to lab-alt-cont-fim
              end-delete
              perform rot-move-cd02
              write reg-cd02 invalid key 
                    move 1 to erro
                    move " Erro de gravacao - ARQCD02A.DAT - Tecle <Ente
      -             "r>" to mensagem
                    display tela-erro
                    perform rot-keypress
                    display tela-limpa
                    go to lab-alt-cont-fim
              end-write
           else
              perform rot-move-cd02
              rewrite reg-cd02 invalid key 
                      move 1 to erro
                      move " Erro de regravacao - ARQCD02A.DAT - Tecle <
      -               "Enter>" to mensagem
                      display tela-erro
                      perform rot-keypress
                      display tela-limpa
                      go to lab-alt-cont-fim
              end-rewrite
           end-if.
           move "Registro alterado - Tecle <Enter>" to mensagem.
           display tela-mensagem-cad-c.
           perform rot-keypress.
      *
       lab-alt-cont-fim.
           unlock arqcd02 record.
           display tela-19.
           exit.