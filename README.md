# Cobol

## Contexto
Essa estação tem o objetivo de prover dados meteorológico para automação de um observatório astronômico. Através de sensores de temperatura, umidade, bârometro, UV, CO2, chuva e luminosidade. Os dados podem ser acessados através de um mini web server que disponibiliza uma API REST

## Tecnologia usada

Desenvolvimento:
> Projeto foi desenvolvido utilizando um microcontrolador Arduino Mega e uma interface ethernet.  O sistema fica em stand by aguardando uma requisição, para fazer a medição e envio dos dados

> A API retorna os dados como um objeto JSON no seguinte formato.

```json
{
  "temperaturaExterna": 21.00,
  "umidade": 51.00,
  "pressao": 949.67,
  "altitude": 543.35,
  "temperaturaInterna": 21.72,
  "uv": 0,
  "co2": 8,
  "chuva": 0,
  "chuvaNivel": 0,
  "luminosidade": 53
}
