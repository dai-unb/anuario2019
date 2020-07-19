### script para tratar os dados do censo

library(tidyverse)

### lê o arquivo que foi importado do CenSup
### http://sistemascensosuperior.inep.gov.br/censosuperior_2018/
### caminho: migração -> exportar dados -> aluno
### colocar o arquivo TXT dentro da pasta "dados_censo"
Censo <- rio::import("dados_censo/Aluno.txt",
                     format = "|",
                     skip = 1,
                     fill = TRUE
)

### separa em arquivo 41 (informação pessoal)
Arquivo41 <- Censo %>%
  filter(V1 == 41) %>%
  select(1:27)

### separa o arquivo 42 (informação do curso)
Arquivo42 <- Censo %>%
  filter(V1 == 42)

### nomeia as variáveis
names(Arquivo41) <- c(
  "Tipo_Reg", "ID_Inep", "Nome", "CPF", "Doc_Est", "Nascimento", "Sexo", "Raca", "Mae", "Nacionalidade", "Cod_UF",
  "Cod_Mun", "Pais", "Deficiente", "Cegueira", "Baixa_Vis", "Surdez", "Def_Aud", "Def_Fis", "Surdocegueira",
  "Def_Mult", "Def_Int", "Autismo", "Sind_Asper", "Sindr_Rett", "Trans_Des", "Super_Dot"
)
names(Arquivo42) <- c(
  "Tipo_Reg", "Sem_Ref", "Cod_Enade", "Cod_Polo_Inep", "MatricAluno", "Turno", "Vinculo", "Curso_Origem",
  "Sem_Conclusao", "Parfor", "Sem_Ingresso", "Tipo_Escola", "Ing_Vest", "Ing_Enem", "Ing_Pas",
  "Ing_Sel", "Ing_Bili", "Ing_PECG", "Ing_Trans", "Ing_Judicial", "Ing_Rem", "Ing_Esp", "Mob_Acad",
  "Tipo_Mob_Acad", "IES_Destino", "Tipo_Mob_Int", "Pais_Dest", "Cotas", "Cota_Etnica", "Cota_Deficientes",
  "Cota_Escola", "Cota_Social", "Cota_Outras", "Fin_Est", "Fin_Fies", "Fin_Estado", "Fin_Municipal", "Fin_Les",
  "Fin_Ext", "Fin_Prounip", "Fin_Exts", "Fin_Estados", "Fin_ies", "Fin_Munics", "Fin_Municss", "Apoio_Social",
  "Apoio_Alimen", "Apoio_Moradia", "Apoio_Trans", "Apoio_Mat_Did", "Apoio_Trab", "Apoio_Perm", "Atv_Ext_Cur",
  "Pesquisa", "Bolsa_Pesquisa", "Extensao", "Bolsa_Extensao", "Monitoria", "Mon_Remun", "EnO", "Bolsa_EnO",
  "CH_Total", "CH_Integral"
)

### cria coluna de identificação,
### usando a ID do CenSup
### primeiro trocando por NA e
### depois substituindo pela valor imediatamente superior
Censo <- Censo %>%
  mutate(
    V2 = as.double(V2),
    V2 = ifelse(nchar(V2) < 10, NA, V2)
  )

repeat.before <- function(x) {
  ind <- which(!is.na(x))
  if (is.na(x[1])) ind <- c(1, ind)
  rep(x[ind], diff(c(ind, length(x) + 1)))
}

Censo$V2 <- repeat.before(Censo$V2)

### mantém os registros relativos ao campo 42
Censo <- Censo %>% filter(V1 == 42)

### cria variável com o identificador INEP
### e arruma a base 42
Arquivo42$ID_Inep <- Censo$V2

Arquivo42 <- Arquivo42 %>%
  select(ID_Inep, Tipo_Reg:CH_Integral) %>%
  mutate(Cod_Enade = as.double(Cod_Enade))

### remove arquivos não mais necessários
rm(Censo, repeat.before)

### arruma o label de algumas variáveis
Arquivo42 <- within(Arquivo42, {
  Vinculo[Vinculo == 2] <- "Cursando"
  Vinculo[Vinculo == 3] <- "Trancado"
  Vinculo[Vinculo == 4] <- "Desvinculado"
  Vinculo[Vinculo == 5] <- "Transferido"
  Vinculo[Vinculo == 6] <- "Formado"
  Vinculo[Vinculo == 7] <- "Falecido"
  
  Turno[Turno == 3] <- "Noturno"
  Turno[Turno == 4] <- "Integral"
  
  Turno[Cod_Enade == 145] <- "Integral" # administração tem apenas integral agora
  
  Ing_PECG[is.na(Ing_PECG)] <- 0
  
  Tipo_Escola[Tipo_Escola == 0] <- "Privado"
  Tipo_Escola[Tipo_Escola == 1] <- "Público"
  Tipo_Escola[Tipo_Escola == 2] <- "Não dispõe de informação"
})

Arquivo41 <- within(Arquivo41, {
  Sexo[Sexo == 0] <- "Masculino"
  Sexo[Sexo == 1] <- "Feminino"
  
  Raca[Raca == 1] <- "Branca"
  Raca[Raca == 2] <- "Preta"
  Raca[Raca == 3] <- "Parda"
  Raca[Raca == 4] <- "Amarela"
  Raca[Raca == 5] <- "Indígena"
  Raca[Raca == 6] <- "Não dispõe de informação"
  Raca[Raca == 0] <- "Aluno não quis declarar cor/raça"
  
  Nome <- as.character(Nome)
  
  Doc_Est <- as.character(Doc_Est)
  
  Mae <- as.character(Mae)
  
  Pais <- as.character(Pais)
})

### carrega arquivos de label
load("dados_censo/Label.RData")
load("dados_censo/Label2.RData")

### Atualizar os Códigos INEP se necessário
Label <- within(Label, {
  Cod_Enade[Cod_Enade == 300152] <- 22851
  Cod_Enade[Cod_Enade == 31380] <- 143
  Cod_Enade[Cod_Enade == 1142454] <- 1140088
  Cod_Enade[Cod_Enade == 29695] <- 26035
})

### agrega as informações dos cursos
Arquivo42 <- inner_join(Arquivo42, Label, "Cod_Enade") %>%
  inner_join(Label2, "Curso")

Arquivo42 <- Arquivo42 %>%
  select(ID_Inep:Cod_Enade, Curso:Local, Cod_Polo_Inep:CH_Integral)

### exporta RDS
saveRDS(Arquivo41, file = "dados_censo/Arquivo41.RDS")
saveRDS(Arquivo42, file = "dados_censo/Arquivo42.RDS")
