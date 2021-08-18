      ***************************************************************
      *                                                             *
      *  A B A V / S P -  I N F O R M A T I C A   :::  PGSL02       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Quadro de Inscricoes :                                     *
      *                                                             *
      *  Data da ultima alteracao:    05/04/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgsl02.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqsl01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is sl01-chave
                  alternate record key is sl01-chave-1 with duplicates
                  alternate record key is sl01-chave-2 with duplicates
                  alternate record key is sl01-chave-3 with duplicates
                  alternate record key is sl01-chave-4 with duplicates
                  file status is sl01-status.
      *
           select arqtxt assign to disk
                  organization is line sequential
                  access mode is sequential
                  file status is txt-status. 
      *
       data division.
       file section.
      *    
       copy fdsl01.lib.
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
       01 sl01-status                  pic x(02) value "00".
       01 sl01-stat                    pic x(01) value "F".
      *
       01 nome-arq-sl01.
          02 sl01-dir                  pic x(03) value "SL1".
          02 filler                    pic x(01) value "\".
          02 sl01-nome                 pic x(08) value "ARQSL01A".
          02 filler                    pic x(01) value ".".
          02 sl01-ext                  pic x(03) value "DAT".
      *
       01 txt-status                   pic x(02) value "00".
       01 txt-stat                     pic x(01) value "F".
      *
       01 nome-arq-txt.
          02 txt-dir                   pic x(03) value "TMP".
          02 filler                    pic x(01) value "\".
          02 txt-nome                  pic x(08) value "ARQTXT02".
          02 filler                    pic x(01) value ".".
          02 txt-ext                   pic x(03) value "DAT".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGSL02".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(45) value spaces.
       01 limpa-aux                    pic x(15) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-fim                     pic x(01) value "N".
       01 campo-dorme                  pic 9(04) comp-5 value 3.
       01 chave-ant                    pic 9(05) value 0.
       01 tracos                       pic x(78) value all "-".
      *
       procedure division.
      *
       lab-00.
           open input arqsl01.
           open output arqtxt.
           move low-values to sl01-chave-1.
           start arqsl01 key is not less sl01-chave-1.
      *
       lab-01.
           read arqsl01 next at end go to lab-fim.
           if sl01-chave = high-values
              go to lab-01
           end-if.
           if sl01-grupo not = "A"
              go to lab-01
           end-if.
           move sl01-empresa-a to txt-empresa.
           move sl01-nome-a to txt-contato.
           move sl01-cargo to txt-cargo.
           move sl01-endereco to txt-endereco.
           move sl01-cep to txt-cep.
           move sl01-uf to txt-uf.
           move sl01-cidade to txt-cidade.
           move sl01-ddd to txt-ddd.
           move sl01-telefone to txt-telefone.
           move sl01-telex to txt-telex.
           move sl01-fax to txt-fax.
           write reg-txt
           go to lab-01.       
      *
       lab-fim.
           close arqsl01 arqtxt.
           stop run.
