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
           select arqtxt assign to disk
                  organization is line sequential
                  access mode is sequential
                  file status is txt-status. 
      *
       data division.
       file section.
      *    
       copy fdcd01.lib.
      *    
       copy fdcd02.lib.
      *
       fd arqtxt
          label record is standard
          value of file-id is nome-arq-txt
          data record is reg-txt.
      *
       01 reg-txt.
          02 txt-empresa               pic x(40).
          02 txt-contato               pic x(40).
          02 txt-cargo                 pic x(15).
          02 txt-endereco              pic x(40).
          02 txt-cep                   pic x(09).
          02 txt-uf                    pic x(02).
          02 txt-cidade                pic x(15).
          02 txt-ddd                   pic x(04).
          02 txt-telefone              pic x(08).
          02 txt-telex                 pic x(08).
          02 txt-fax                   pic x(08).
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
       01 txt-status                   pic x(02) value "00".
       01 txt-stat                     pic x(01) value "F".
      *
       01 nome-arq-txt.
          02 txt-dir                   pic x(03) value "TMP".
          02 filler                    pic x(01) value "\".
          02 txt-nome                  pic x(08) value "ARQTXT01".
          02 filler                    pic x(01) value ".".
          02 txt-ext                   pic x(03) value "DAT".
      *
      *
       01 limpa-45                     pic x(45) value spaces.
       01 limpa-15       	       pic x(15) value spaces.
       01 limpa-08                     pic x(08) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-empresa                 pic x(01) value "N".
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 categoria                    pic 9(03) value 0.
      *
       procedure division.
      *
       lab-00.
           open input arqcd01 arqcd02.
           open output arqtxt.
           accept categoria.
           move low-values to cd01-chave-1.
           start arqcd01 key is not less cd01-chave-1.
      *
       lab-01.
           read arqcd01 next at end go to lab-fim.
           if cd01-chave = high-values
              go to lab-01
           end-if.
           if cd01-categoria not = categoria
              go to lab-01
           end-if.
           move cd01-codigo to cd02-codigo.
           move 01 to cd02-contato.
           read arqcd02 invalid key move spaces to cd02-nome-a.
           move cd01-nome-fantasia-a to txt-empresa.
           move cd02-nome-a to txt-contato.
           move spaces to txt-cargo.
           move cd01-endereco to txt-endereco.
           move cd01-uf to txt-uf.
           move cd01-cep to txt-cep.
           move cd01-cidade to txt-cidade.
           move cd01-ddd to txt-ddd.
           move cd01-telefone to txt-telefone.
           move cd01-telex to txt-telex.
           move cd01-fax to txt-fax.
           write reg-txt
           go to lab-01.       
        
      *
       lab-fim.
           close arqcd01 arqcd02 arqtxt.
           stop run.
