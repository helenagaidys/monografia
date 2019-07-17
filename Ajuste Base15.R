library(survey)

pns_dat <- readRDS('/Users/helenaboingaidys/PNS/2013 long questionnaire survey.rds')
pns_calib <- readRDS('/Users/helenaboingaidys/PNS/2013 long questionnaire survey design.rds')

#### Funções ####

#Calcula proporção de cada observação para cada variável e erro padrão %
prop_rel <- function(y) round(100* coef(y),1)

#Estimativa da proporção de casos z com diagnóstico de depressão %
stats <- function(z) round(100* c(coef(z), coef(z) - 2 * SE(z), coef(z) + 2 * SE(z)),1)

##### Novas Variáveis #####

df_renda = cbind(pns_calib$variables$e01602, pns_calib$variables$e01604, pns_calib$variables$e01802, 
                 pns_calib$variables$e01804, pns_calib$variables$f00102, pns_calib$variables$f00702, 
                 pns_calib$variables$f00802, pns_calib$variables$vdf00102)

pns_calib <- update (pns_calib,
diag_dep = as.numeric(q092=='1'),
renda = rowSums(df_renda, na.rm = TRUE),
estado = factor(v0001, labels = c('RO','AC','AM','RR','PA','AP','TO','MA','PI','CE','RN','PB','PE','AL','SE',
                                  'BA','MG','ES','RJ','SP','PR','SC','RS','MS','MT','GO','DF')),
sex = factor(c006, labels = c('Masculino','Feminino'), ordered = TRUE),
plan_saude = factor(i001, labels = c('Com Plano de Saúde','Sem Plano de Saúde'), ordered = TRUE),
idade =  as.numeric(c008),
empr = as.numeric(e001=="1" | e005=="1"),
empr = factor(empr, labels = c('Desempregado', 'Empregado'), ordered = TRUE),
doenca = as.numeric( q002 == '1' | q030 == '1' | q060 == '1' | q063 == '1' |
                       q068 == '1' | q074 == '1' | q079 == '1' | q088 == '1' |
                       q116 == '1' | q124 == '1' | q128 == '1'),
doenc = factor(doenca, labels = c('Sem Doença Crônica', 'Com Doença Crônica'), ordered = TRUE),
renda4 = cut(renda, breaks = quantile(renda,(0:4)/4))
)

rm(df_renda)

#### Retirada de Outliers ####

new_pns_calib <- subset(pns_calib, idade>=18 & idade<=80 & raca!="Indigena" & renda <=2034)

new_pns_calib <- update(new_pns_calib,
renda3 = cut(renda, breaks = quantile(renda,(0:3)/3), labels = c('Até 1 Salário Mín.','Até 2 Salário Mín.',
                                                                 'Até 3 Salário Mín.'))
)

#### Estatísticas Preliminares ####

depr_rate <- svymean(~diag_dep,design = new_pns_calib, na.rm=TRUE)

depratebr <- prop_rel(depr_ratebr)

depr_ratebr <- svymean(~diag_dep,design = pns_calib, na.rm=TRUE)

depratebr <- prop_rel(depr_ratebr)

depr_ratecplan <- svymean(~diag_dep,design = subset(pns_calib, plan_saude == 'Sem Plano de Saúde'), na.rm=TRUE)

depratecplan <- prop_rel(depr_ratecplan)

plansaudet <- svymean(~plan_saude,design = pns_calib, na.rm=TRUE)
PRplansaudet <- prop_rel(plansaudet)
plansaude_dept <- svymean(~plan_saude,design = subset(pns_calib, diag_dep==1), na.rm = TRUE)
PRplansaudedept <- prop_rel(plansaude_dept)


#### Sexo ####


#proporção de pessoas com diagnóstico de depressão por sexo
sex_dep <- svyby(~diag_dep,~sex,design = pns_calib,svymean, na.rm = TRUE)

est_sexdep <- stats(sex_dep)

df_sexdep <- data.frame('Variáveis' = names(est_sexdep[1:2]),
                        'Prop.' = est_sexdep[1:2],
                        'L.I.C' = est_sexdep[3:4],
                        'L.S.C' = est_sexdep[5:6])

#proporção de homens e mulheres na amostra
sexbr <- svymean(~sex,design = pns_calib, na.rm=TRUE)

PRsex <- prop_rel(sexbr)

df_sex <- data.frame('Variável' = c('Masculino','Feminino'),
                     'Perc.' = PRsex[1:2],
                     'Prop.' = df_sexdep[,2],
                     'L.I.C' = df_sexdep[,3],
                     'L.S.C' = df_sexdep[,4])

rm(sex_dep)
rm(sexbr)
rm(est_sexdep)
rm(PRsex)

#### Capital ####


cap <- svyby(~diag_dep,~estado,design = subset(pns_calib, v0031 == '1' & (estado == 'SP' |
                                                         estado == 'RJ' | estado == 'MG' | estado == 'RS' |
                                                         estado == 'PE' | estado == 'CE' | estado == 'BA' |
                                                         estado == 'PR')), 
                  svymean, na.rm = TRUE)

est_cap <- stats(cap)

df_cap <- data.frame('Variáveis' = c('FOR','REC','SSA','BH','RJ','SP','CTB','POA'),
                        'Prop.' = est_cap[1:8],
                        'L.I.C' = est_sexcap[9:16],
                        'L.S.C' = est_sexcap[17:24])

#proporção de pessoas em cada capital na amostra
capbr <- svymean(~estado,design = subset(pns_calib, v0031 == '1' 
                                         & (estado == 'SP' |estado == 'RJ' | estado == 'MG' | 
                                            estado == 'RS' |estado == 'PE' | estado == 'CE' | 
                                            estado == 'BA' |estado == 'PR')),
                 na.rm=TRUE)

PRcap <- prop_rel(capbr)

df_PRcap <- data.frame('Perc.' = PRcap[c(10,13,16,17,19,20,21,23)])

df_capital <- data.frame('Variável' = c('FOR','REC','SSA','BH','RJ','SP','CTB','POA'),
                     'Perc.' = df_PRcap[,1],
                     'Prop.' = df_cap[,2],
                     'L.I.C' = df_cap[,3],
                     'L.S.C' = df_cap[,4])

rm(cap)
rm(est_cap)
rm(capbr)
rm(PRcap)
rm(df_PRcap)

#### Plano de Saúde ####

#proporção de pessoas com diagnóstico de depressão que tem ou não plano de saúde
plansaude_dep <- svyby(~diag_dep,~plan_saude,design = pns_calib, svymean, na.rm = TRUE)

est_plansaudedep <- stats(plansaude_dep)

df_plansaudedep <- data.frame('Variáveis' = names(est_plansaudedep[1:2]),
                              'Prop.' = est_plansaudedep[1:2],
                              'L.I.C.' = est_plansaudedep[3:4],
                              'L.S.C.' = est_plansaudedep[5:6])

#proporção de pessoas por estado que tem plano de saúde
plansaude_r <- svyby(~as.numeric(plan_saude == 'Com Plano de Saúde'),~region,design = pns_calib,
                       svymean, na.rm=TRUE)

est_plansauder <- stats(plansaude_r)

df_plansaudecap <- data.frame('Variáveis' = names(est_plansauder)[1:5],
                              'Prop.' = est_plansauder[1:5],
                              'L.I.C.' = est_plansauder[6:10],
                              'L.S.C.' = est_plansauder[11:15])

#proporção de pessoas que tem plano de saúde ou não, por sexo
plansaudebr <- svymean(~plan_saude,design = pns_calib, na.rm=TRUE)

PRplansaude <- prop_rel(plansaudebr)

df_PRplansaude <- data.frame('Perc.' = PRplansaude[1:2])

df_plansaude <- data.frame('Variável' = c('Com Plano de Saúde', 'Sem Plano de Saúde'),
                           'Perc.' = df_PRplansaude[,1],
                           'Prop.' = df_plansaudedep[,2],
                           'L.I.C' = df_plansaudedep[,3],
                           'L.S.C' = df_plansaudedep[,4])

rm(PRplansaude)
rm(plansaude_dep)
rm(est_plansaudedep)
rm(df_PRplansaude)
rm(df_plansaudedep)
rm(plansaudebr)
rm(plansaude_r)
rm(est_plansauder)

#### Idade ####


#proporção de pessoas com diagnóstico de depressão por idade e por sexo
idade_dep <- svyby(~diag_dep,~age_cat,design = pns_calib, svymean, na.rm = TRUE)

est_idadedep <- stats(idade_dep)

df_idadedep <- data.frame('Variáveis' = names(est_idadedep[1:4]),
                              'Prop.' = est_idadedep[1:4],
                              'L.I.C.' = est_idadedep[5:8],
                              'L.S.C.' = est_idadedep[9:12])

#proporção de idade na amostra
idadebr <- svymean(~age_cat,design = pns_calib, na.rm=TRUE)

PRidade <- prop_rel(idadebr)

df_PRidade <- data.frame('Perc.' = PRidade[2:5])


df_idade <- data.frame('Variável' = c('18-29', '30-39', '40-59', '60+'),
                           'Perc.' = df_PRidade[,1],
                           'Prop.' = df_idadedep[,2],
                           'L.I.C' = df_idadedep[,3],
                           'L.S.C' = df_idadedep[,4])

rm(PRidade)
rm(idade_dep)
rm(est_idadedep)
rm(df_PRidade)
rm(df_idadedep)
rm(idadebr)

#### Residência ####

#proporção de pessoas com diagnóstico de depressão por zona de residência, por sexo
situ_dep <- svyby(~diag_dep,~situ,design = pns_calib, svymean, na.rm = TRUE)

est_situdep <- stats(situ_dep)

df_situdep <- data.frame('Variáveis' = names(est_situdep[1:2]),
                           'Prop.' = est_situdep[1:2],
                           'L.I.C.' = est_situdep[3:4],
                           'L.S.C.' = est_situdep[5:6])


#proporção de pessoas em cada zona de residência na amostra
situbr <- svymean(~situ,design = pns_calib, na.rm=TRUE)

PRsitu <- prop_rel(situbr)

df_PRsitu <- data.frame('Perc.' = PRsitu[1:2])

df_situ <- data.frame('Variável' = c('R. Urbana', 'R. Rural'),
                       'Perc.' = df_PRsitu[,1],
                       'Prop.' = df_situdep[,2],
                       'L.I.C' = df_situdep[,3],
                       'L.S.C' = df_situdep[,4])

rm(PRsitu)
rm(situ_dep)
rm(est_situdep)
rm(df_PRsitu)
rm(df_situdep)
rm(situbr)

#### Região ####

#proporção de pessoas com diagnóstico de depressão por região
region_dep <- svyby(~diag_dep,~region,design = pns_calib, svymean, na.rm = TRUE)

est_regiondep <- stats(region_dep)

df_regiondep <- data.frame('Variáveis' = names(est_regiondep[1:5]),
                           'Prop.' = est_regiondep[1:5],
                           'L.I.C.' = est_regiondep[6:10],
                           'L.S.C.' = est_regiondep[11:15])

#proporção de pessoas por região na amostra
regionbr <- svymean(~region,design = pns_calib, na.rm=TRUE)

PRregion <- prop_rel(regionbr)

df_PRregion <- data.frame('Perc.' = PRregion[1:5])

df_region <- data.frame('Variável' = c('Norte', 'Nordeste', 'Sudeste','Sul', 'Centro-Oeste'),
                       'Perc.' = df_PRregion[,1],
                       'Prop.' = df_regiondep[,2],
                       'L.I.C' = df_regiondep[,3],
                       'L.S.C' = df_regiondep[,4])

rm(PRregion)
rm(region_dep)
rm(est_regiondep)
rm(df_PRregion)
rm(df_regiondep)
rm(regionbr)

#### Renda Individual ####

#proporção de pessoas com diagnóstico de depressão por quartil de renda, por sexo
renda4_dep <- svyby(~diag_dep,~renda4,design = pns_calib, svymean, na.rm = TRUE)

est_renda4dep <- stats(renda4_dep)

df_renda4dep <- data.frame('Variáveis' = names(est_renda4dep[1:4]),
                           'Prop.' = est_renda4dep[1:4],
                           'L.I.C.' = est_renda4dep[5:8],
                           'L.S.C.' = est_renda4dep[9:12])

#proporção de pessoas por quartis de renda na amostra
renda4br <- svymean(~renda4,design = pns_calib, na.rm=TRUE)

PRrenda4 <- prop_rel(renda4br)

df_PRrenda4 <- data.frame('Perc.' = PRrenda4[1:4])

df_renda4 <- data.frame('Variável' = names(est_renda4dep[1:4]),
                       'Perc.' = df_PRrenda4[,1],
                       'Prop.' = df_renda4dep[,2],
                       'L.I.C' = df_renda4dep[,3],
                       'L.S.C' = df_renda4dep[,4])

rm(PRrenda4)
rm(renda4_dep)
rm(est_renda4dep)
rm(df_PRrenda4)
rm(df_renda4dep)
rm(renda4br)

#### Emprego ####

empr_dep <- svyby(~diag_dep,~empr,design = pns_calib, svymean, na.rm = TRUE)
est_emprdep <- stats(empr_dep)

df_emprdep <- data.frame('Variáveis' = names(est_emprdep[1:2]),
                               'Prop.' = est_emprdep[1:2],
                               'L.I.C.' = est_emprdep[3:4],
                               'L.S.C.' = est_emprdep[5:6])

empr_br <- svymean(~empr,design = pns_calib, na.rm=TRUE)
PRempr <- prop_rel(empr_br)

df_PRempr <- data.frame('Perc.' = PRempr[1:2])

df_empr <- data.frame('Variável' = c('Desempregado', 'Empregado'),
                            'Perc.' = df_PRempr[,1],
                            'Prop.' = df_emprdep[,2],
                            'L.I.C' = df_emprdep[,3],
                            'L.S.C' = df_emprdep[,4])
rm(PRempr)
rm(empr_dep)
rm(est_emprdep)
rm(df_PRempr)
rm(df_emprdep)
rm(empr_br)

### Doenças Crônicas ####

doenc_dep <- svyby(~diag_dep,~doenc,design = pns_calib, svymean, na.rm = TRUE)
est_doencdep <- stats(doenc_dep)

df_doencdep <- data.frame('Variáveis' = names(est_doencdep[1:2]),
                          'Prop.' = est_doencdep[1:2],
                          'L.I.C.' = est_doencdep[3:4],
                          'L.S.C.' = est_doencdep[5:6])

doencbr <- svymean(~doenc,design = pns_calib, na.rm=TRUE)
PRdoenc <- prop_rel(doencbr)

df_PRdoenc <- data.frame('Perc.' = PRdoenc[1:2])

df_doenc <- data.frame('Variável' = c('Sem Doenc. Cron.', 'Com Doenc. Cron.'),
                       'Perc.' = df_PRdoenc[,1],
                       'Prop.' = df_doencdep[,2],
                       'L.I.C' = df_doencdep[,3],
                       'L.S.C' = df_doencdep[,4])

rm(PRdoenc)
rm(doenc_dep)
rm(est_doencdep)
rm(df_PRdoenc)
rm(df_doencdep)
rm(doencbr)
                          
#### Tabela Final ####

#Concatenar todos os vetores para poder fazer as tabelas

df_depr <- rbind(df_plansaude,df_sex,df_idade,
                 df_situ,df_region, df_empr,df_doenc)
names(df_depr) <- c('Variável', 'Perc.', 'Prop.', 'L.I.C', 'L.S.C')
row.names(df_depr) <- NULL

df_plansaudecap
PRdepr_rate

library(knitr)
library(kableExtra)

options(knitr.table.format = "latex")

tab1 <- knitr::kable(df_depr, booktabs=TRUE, digits = 2,
  caption = 'Percentuais, Proporção de Depressão, Limite Inferior de Confiança de 95, 
  Limite Superior de Confiança de 95 - PNS 2013 ()') %>%
  add_footnote("Fonte: Elaborado pelo autor com os dados da PNS 2013") %>%
  kable_styling(font_size = 10, position = 'c') %>%
  group_rows('Plano de Saúde',1,2) %>%
  group_rows('Sexo',3,4) %>%
  group_rows('Idade',5,8) %>%
  group_rows('Zona Res.',9,10) %>%
  group_rows('Região',11,15) %>%
  group_rows('Trab. Remunerado',16,17) %>%
  group_rows('Doenças Crôn.',18,19)


#### Testes de Independência ####

# svychisq(~plan_saude + sex, design = pns_calib)
# svychisq(~plan_saude + idade, design = pns_calib)  
# svychisq(~plan_saude + tam_fam, design = pns_calib)  
# #svychisq(~plan_saude + renda, design = pns_calib)  
# svychisq(~plan_saude + casado, design = pns_calib)
# svychisq(~plan_saude + chefe, design = pns_calib)
# svychisq(~plan_saude + empr, design = pns_calib)
# svychisq(~plan_saude + doenc, design = pns_calib)
# svychisq(~plan_saude + raca, design = pns_calib)
# svychisq(~plan_saude + educ, design = pns_calib)
# svychisq(~plan_saude + region, design = pns_calib)
# svychisq(~plan_saude + situ, design = pns_calib)
# svychisq(~trat_depr + sex, design = pns_calib)
# svychisq(~trat_depr + idade, design = pns_calib)
  
#### Regressões ####

library(margins)

# variancia <- svyvar(~as.numeric(plan_saude) + as.numeric(sex), design = new_pns_calib)
# variancia <- as.matrix(variancia)
# correlacao <- cov2cor(variancia)
# correlacao <- correlacao[1:nrow(correlacao), 1:nrow(correlacao)]
# correlacao

reg_pns_calib <- update(new_pns_calib,
                        plan_saude = ordered(plan_saude, levels = c('Sem Plano de Saúde','Com Plano de Saúde')),
                        sex = ordered(sex, levels = c('Feminino','Masculino')),
                        situ = ordered(situ, levels = c('rural','urbano')),
                        region = ordered(region, levels = c('Sudeste','Norte','Nordeste','Sul','Centro-Oeste'))
                        )

#BIC plan_saude = 29677.39 #
#BIC empr = 29659.36
#BIC doenc = 29422.48
#BIC doenc + empr = 29411.43 #
#BIC doenc + empr + idade + idade2 = 29391.47
#BIC doenc + empr + idade + idade2 + sex = 29248.56 #
#BIC idade + idade 2 + sex = 29485.67
#BIC doenc + empr + idade + idade2 + sex + region = 29046.31 #
#BIC doenc + empr + idade + idade2 + sex + situ = 29233.09 #
#BIC doenc + empr + situ = 29383.48
#BIC doenc + empr + region = 29209.48
#BIC empr + region = 29492.99
#BIC idade + idade2 + sex + situ = 29475.92
#BIC idade + idade2 + sex + region = 29343.41
#BIC doenc + empr + idade + idade2 + sex + region + situ = 29042.18 #

#1 61483.09
# empr 61421.59
# doenc 61459.45
# situ 61019.02
# region 60985.09
# region + empr 60962.9
# region + empr + doenc 60944.57
# region + empr + doenc + situ 60604.55

library(poliscidata)

#Regressão Probit de diagnóstico de depressão
reg <- svyglm(diag_dep ~ plan_saude,
              design = reg_pns_calib, 
              family = quasibinomial(link= 'probit'))

regm <- margins(reg, design = reg_pns_calib)

fit.svyglm(reg)
psrsq(reg)
summary(reg)
regTermTest(reg,~plan_saude, method = 'Wald')

#Regressão Probit de diagnóstico de depressão com a inclusão de 4 variáveis correlacionadas com plano de saúde
reg1 <- svyglm(diag_dep ~ plan_saude + doenc + empr + region + situ,
                design = reg_pns_calib, 
                family = quasibinomial(link= 'probit'))

BIC(reg1,
    maximal = svyglm(diag_dep ~ plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ + region,
                     design = reg_pns_calib, 
                     family = quasibinomial(link= 'probit')))
#fit.svyglm(reg1)
psrsq(reg1)
#summary(reg1m)
regTermTest(reg1,~ plan_saude + doenc + empr + region + situ, method = 'Wald')

reg1m <- margins(reg1, design = reg_pns_calib)
knitr::kable(summary(reg1m),booktabs=TRUE, digits = 4,
             caption = 'Efeito de plano de saúde e de outros fatores na probabilidade 
             de desenvolver transtorno depressivo') %>%
  kable_styling(font_size = 10, position = 'c') %>%
  add_footnote(c('Observações = 5429','Pseudo R-Quadrado = 0.04601','BIC = 29195.89','Wald(g.l) = 91.15096(8)'))


#Inclusão de 2 variáveis de controle
reg2 <- svyglm(diag_dep ~ plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ + region,
               design = reg_pns_calib, 
               family = quasibinomial(link= 'probit'))

BIC(reg2,
    maximal = svyglm(diag_dep ~ plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ + region,
                     design = reg_pns_calib, 
                     family = quasibinomial(link= 'probit')))

psrsq(reg2)
regTermTest(reg2,~plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ + region, method = 'Wald')
reg2m <- margins(reg2, design = reg_pns_calib)
knitr::kable(summary(reg2m),booktabs=TRUE, digits = 4,
             caption = 'Efeito de plano de saúde e de outros fatores na probabilidade 
             de desenvolver transtorno depressivo') %>%
  kable_styling(font_size = 10, position = 'c') %>%
  add_footnote(c('Observações = 5427','Pseudo R-Quadrado = 0.06132','BIC = 29042.18','Wald(g.l) = 68.67967(11)'),
               notation = "none")

#Suposições:
#Indício de má qualidade do sistema público nas capitais em comparação a fora das capitais (já que lá o sistema
# de saúde privada mostra-se mais significante que fora das capitais).
#Indício de falta de profissionais da saúde privada nas áreas fora das capitais.
#Indício de subestimação de diagnósticos de depressão nas áreas fora das capitais.


reg3 <- svyglm(diag_dep ~ plan_saude + doenc + empr + idade + sqrt(idade) + sex + situ,
               design = subset(reg_pns_calib, region == 'Nordeste'), 
               family = quasibinomial(link= 'probit'))
BIC(reg3,maximal = svyglm(diag_dep ~ plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ + region,
                     design = reg_pns_calib, 
                     family = quasibinomial(link= 'probit')))
psrsq(reg3)
regTermTest(reg3,~plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ, method = 'Wald')
names(reg3$coefficients) <- c('plan_saude','empr','doenc','idade',
                              'idade2','sex.masc','situ.urb')
reg3m <- margins(reg3, design = reg_pns_calib)
knitr::kable(summary(reg3m),booktabs=TRUE, digits = 4,
             caption = 'Efeito de plano de saúde e de outros fatores na probabilidade 
             de desenvolver transtorno depressivo') %>%
  kable_styling(font_size = 10, position = 'c') %>%
  add_footnote(c('Observações = 1706','Pseudo R-Quadrado = 0.03507','BIC = 29224.156','Wald(g.l) = 36.16607(7)'),
               notation = "none")

reg4 <- svyglm(diag_dep ~ plan_saude + doenc + empr + idade + sqrt(idade) + sex + situ,
               design = subset(reg_pns_calib, region == 'Sudeste'), 
               family = quasibinomial(link= 'probit'))

BIC(reg4,maximal = svyglm(diag_dep ~ plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ + region,
                          design = reg_pns_calib, 
                          family = quasibinomial(link= 'probit')))
psrsq(reg4)
regTermTest(reg4,~plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ, method = 'Wald')
names(reg4$coefficients) <- c('plan_saude','empr','doenc','idade',
                              'idade2','sex.masc','situ.urb')
reg4m <- margins(reg4, design = reg_pns_calib)
knitr::kable(summary(reg4m),booktabs=TRUE, digits = 4,
             caption = 'Efeito de plano de saúde e de outros fatores na probabilidade 
             de desenvolver transtorno depressivo') %>%
  kable_styling(font_size = 10, position = 'c') %>%
  add_footnote(c('Observações = 1274','Pseudo R-Quadrado = 0.05638','BIC = 29221.070','Wald(g.l) = 25.43336(7)'),
               notation = "none")

reg5 <- svyglm(diag_dep ~ plan_saude + doenc + empr + idade + sqrt(idade) + sex + situ,
               design = subset(reg_pns_calib, region == 'Sul'), 
               family = quasibinomial(link= 'probit'))

BIC(reg5,maximal = svyglm(diag_dep ~ plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ + region,
                          design = reg_pns_calib, 
                          family = quasibinomial(link= 'probit')))
psrsq(reg5)
regTermTest(reg5,~plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ, method = 'Wald')
names(reg5$coefficients) <- c('plan_saude','empr','doenc','idade',
                              'idade2','sex.masc','situ.urb')
reg5m <- margins(reg5, design = reg_pns_calib)
knitr::kable(summary(reg5m),booktabs=TRUE, digits = 4,
             caption = 'Efeito de plano de saúde e de outros fatores na probabilidade 
             de desenvolver transtorno depressivo') %>%
  kable_styling(font_size = 10, position = 'c') %>%
  add_footnote(c('Observações = 661','Pseudo R-Quadrado = 0.09018','BIC = 29215.690','Wald(g.l) = 30.84159(7)'),
               notation = "none")


reg6 <- svyglm(diag_dep ~ plan_saude + doenc + empr + idade + sqrt(idade) + sex + situ,
               design = subset(reg_pns_calib, region == 'Centro-Oeste'), 
               family = quasibinomial(link= 'probit'))

BIC(reg6,maximal = svyglm(diag_dep ~ plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ + region,
                          design = reg_pns_calib, 
                          family = quasibinomial(link= 'probit')))
psrsq(reg6)
regTermTest(reg6,~plan_saude + empr + doenc + idade + sqrt(idade) + sex + situ, method = 'Wald')
names(reg6$coefficients) <- c('plan_saude','empr','doenc','idade',
                              'idade2','sex.masc','situ.urb')
reg6m <- margins(reg6, design = reg_pns_calib)
knitr::kable(summary(reg6m),booktabs=TRUE, digits = 4,
             caption = 'Efeito de plano de saúde e de outros fatores na probabilidade 
             de desenvolver transtorno depressivo') %>%
  kable_styling(font_size = 10, position = 'c') %>%
  add_footnote(c('Observações = 694','Pseudo R-Quadrado = 0.05001','BIC = 29216.124','Wald(g.l) = 17.90299(7)'),
               notation = "none")

#Apenas para o Nordeste ter plano de saúde aumenta a probabilidade de diagnóstico positivo para depressão


summary(svyglm(plan_saude ~ doenc + situ + region + idade + sex + empr,
       design = pns_calib, 
       family = quasibinomial(link= 'probit')))
plot(svyglm(plan_saude ~ doenc + situ + region + idade + sex + empr,
       design = pns_calib, 
       family = quasibinomial(link= 'probit')))
