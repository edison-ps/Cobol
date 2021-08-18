      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  QGAG01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Conversao Args. GTP / Agenda :                             *
      *                                                             *
      *  Data da ultima alteracao:    26/07/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. qgag01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqag01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is ag01-chave
                  alternate record key is ag01-chave-1 with duplicates
                  alternate record key is ag01-chave-2 with duplicates
                  alternate record key is ag01-chave-3 with duplicates
                  alternate record key is ag01-chave-4 with duplicates
                  alternate record key is ag01-chave-5 with duplicates
                  file status is ag01-status.
      *
           select arqgtp assign to disk
                  organization is line sequential
                  access mode is sequential
                  file status is gtp-status.
      *
       data division.
       file section.
      *    
       copy fdag01.lib.
      *
       copy fdgtp.lib.
      *
       working-storage section.
      *
       01 ag01-status                  pic x(02) value "00".
       01 ag01-stat                    pic x(01) value "F".
      *
       01 nome-arq-ag01.
          02 ag01-dir                  pic x(03) value "AG2".
          02 filler                    pic x(01) value "\".
          02 ag01-nome-arq             pic x(08) value "ARQAG01A".
          02 filler                    pic x(01) value ".".
          02 ag01-ext                  pic x(03) value "DAT".
      *
       01 gtp-status                   pic x(02) value "00".
       01 gtp-stat                     pic x(01) value "F".
      *
       01 nome-arq-gtp                 pic x(30) value spaces.
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "QGAG01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(50) value spaces.
       01 limpa-03                     pic x(03) value spaces.
       01 limpa-10                     pic x(10) value spaces.
       01 limpa-20                     pic x(20) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 codigo                       pic 9(05) value 0.
      *
       01 data-accept                  pic 9(06) value 0.
       01 data-edit redefines data-accept.
          02 edit-ano                  pic 9(02).
          02 edit-mes                  pic 9(02).
          02 edit-dia                  pic 9(02).
      *
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       copy workgen.lib.
      *
       screen section.
      *
       copy scrgen.lib.
      * 
       procedure division.
      *
       lab-01.
           call "C_Cls".
           accept data-accept from date.
           move edit-ano to ano-euro.
           move edit-mes to mes-euro.
           move edit-dia to dia-euro.
           move "4" to opcao-data.
           perform rot-data.
           accept nome-arq-gtp with prompt.
           perform sec-converte.
           stop run.
      *
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *      
       rotinas section.
      *
       rot-move-ag01.
           move codigo to ag01-codigo.
           move "INICIALIZACAO" to ag01-assunto ag01-assunto-a.
           move gtp-empresa to ag01-empresa-a txt.
           perform rot-texto.
           move txt to ag01-empresa.
           move gtp-nome to ag01-nome-a txt.
           perform rot-texto.
           move txt to ag01-nome.
           move gtp-partido to ag01-partido-a txt.
           perform rot-texto.
           move txt to ag01-partido.
           move dias-corr to ag01-data-a.
           move gtp-endereco to ag01-endereco.
           move 70150900 to ag01-cep.
           move "BRASILIA" to ag01-cidade.
           move gtp-uf to ag01-uf.
           move gtp-ddd to ag01-ddd.
           move gtp-telefone to ag01-telefone.
           move spaces to ag01-telex.
           move gtp-fax to ag01-fax.
           move "N" to ag01-posicao.
           move "AUTO" to ag01-usuario.
           move dias-corr to ag01-data.
      *
       rot-ponteiro.
           move 0 to erro.
           start arqag01 key is equal ag01-chave invalid key
                 move 1 to erro
      *           perform rot-erro-leitura-ag01
           end-start.
      *
       rot-le-ag01-lock.
           move 0 to erro.
           read arqag01 next. 
           if ag01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait-aux
              go to rot-le-ag01-lock
           end-if.
           read arqag01 with kept lock.
      *
       rot-open-ag01.
           move 0 to erro.
           if ag01-stat = "F"
              open i-o arqag01
              if ag01-status not = "00"
                 move 
                 " Erro de abertura no ARQAG01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 move 1 to erro
                 move zeros to reg-ag01
                 move high-values to ag01-controle
                 move 0 to ag01-numero
                 write reg-ag01-1
               else
                  move "A" to ag01-stat
               end-if
           end-if.
      *
       rot-close-ag01.
           if ag01-stat = "A"
              close arqag01
              move "F" to ag01-stat
           end-if.
      *
       rot-open-gtp.
           move 0 to erro.
           if gtp-stat = "F"
              open input arqgtp
              if gtp-status not = "00"
                 move 
                 " Erro de abertura no ARQ.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 move 1 to erro
               else
                  move "A" to gtp-stat
               end-if
           end-if.
      *
       rot-close-gtp.
           if gtp-stat = "A"
              close arqgtp
              move "F" to gtp-stat
           end-if.
      *
       copy rotgen.lib.
      *
       sec-converte section.
      *
       lab-conv-00.
           perform rot-open-ag01.
           if erro not = 0
              go to lab-conv-fim
           end-if.
           perform rot-open-gtp.
           if erro not = 0
              go to lab-conv-fim
           end-if.
      *
       lab-conv-01.
           move 0 to erro.
           read arqgtp at end move 1 to erro.
           if erro not = 0
              go to lab-conv-fim
           end-if.
      *
       lab-conv-02.
           move high-values to ag01-chave-controle.
           perform rot-ponteiro.
           perform rot-le-ag01-lock.
           if erro not = 0
              move " Erro no registro de controle - ARQAG01A.DAT - Tecle
      -       " <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              go to lab-conv-fim
           end-if.
           move ag01-numero to codigo.
           add 1 to ag01-numero codigo.
           rewrite reg-ag01.
           unlock arqag01 record.
           perform rot-move-ag01.
           display reg-ag01 " " "--->" reg-gtp
           write reg-ag01 invalid key 
                 move 1 to erro
                 move " Erro de gravacao - ARQAG01A.DAT - Tecle <Enter>"
                 to mensagem
                 display tela-erro
                 perform rot-keypress
                 go to lab-conv-fim
           end-write.
           display ag01-codigo.
           go to lab-conv-01.
      *
       lab-conv-fim.
           perform rot-close-ag01.
           perform rot-close-gtp.
           exit.
