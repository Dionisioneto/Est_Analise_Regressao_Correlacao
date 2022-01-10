
##---
## Lista 4
##---

## Vetor da variável independente, somado ao vetor com uns como intercepto.
x = matrix(data = c(c(1,1,1,1,1),c(8,4,0,-4,-8)), nrow = 5, ncol = 2)
x
## Vetor da variável resposta.
y = c(7.8, 9, 10.2, 11, 11.7)
y

## Formula
## t((x) * x)^(-1) * (t(x) * y)

## t((x) * x)
(t(x) %*% x)

## Inversa de t((x) * x)
(solve(t(x) %*% x))

## (t(x) * y)
(t(x) %*% y)

## Gera os interceptos da regressão
interceptos <- (solve(t(x) %*% x)) %*% (t(x) %*% y)
interceptos

##---Armazenando os vetores---

## Beta0
beta0 <- interceptos[1,1]

## Beta1
beta1 <- interceptos[2,1]

## Predição dos valores
y_pred <- beta0 + beta1*x[,2]
y_pred

## Vetor de resíduos

e <- y - y_pred
e

## Soma do quadrado dos resíduos
e_quad <- (e)^2
soma_e_quad <- sum(e_quad)

## Matriz de variancia e covariancia
## Fórmula

## sigma^2 * solve(t(x) %*% x)
## Onde: sigma^2 = SMR / (n-k)
solve(t(x) %*% x)

sigma_quad <- soma_e_quad/ (length(y) - 2)
sigma_quad

## Matriz de var-cov
var_cov <- sigma_quad * solve(t(x) %*% x)
var_cov

## Erro-padrão dos coeficientes
sqrt(var_cov)

##---
## Aplicando com a funcao lm no R
## Esta funcao ja raliza tudo no software
##---

##---Salvando os valores---
x <- c(8,4,0,-4,-8)
y <- c(7.8,9,10.2,11,11.7)

##---Treinando o modelo---
mod_simples <- lm(y ~ x)

###--- Resumo das informacoes---
summary(mod_simples)
mod_simples$residuals
mod_simples$fitted.values


