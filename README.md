# Аннотация
В данной работе сравниваются прогнозы, построенные с помощью векторной байесовской авторегрессии с переменными параметрами, с прогнозами одномерных моделей и их комбинациями. В качестве одномерных оппонентов векторной авторегрессии были выбраны такие модели как ARIMA, экспоненциальное сглаживание и тета-метод. Данные, на которых оценивались модели, состоят из 9 месячных макроэкономических временных рядов и 24 рядов с дневными приростами цен акций крупнейших российских компаний. Горизонты прогноза составляли от 3 до 24 месяцев для месячных и от 7 до 260 дней для ежедневных рядов.
Для сравнения полученных прогнозов использовалась относительная ошибка прогноза, которая рассчитывалась как отношение среднеквадратичной ошибки прогноза модели к среднеквадратичной ошибке прогноза модели случайного блуждания.
В результате проведенного анализа были получены следующие выводы: прогнозы TVP-BVAR оказались ближе всего к истинным значениям ровно в половине случаев для макроэкономических данных и более чем в трети случаев для финансового набора данных. Тем не менее, было замечено, что для финансовых данных разница в относительных ошибках незначительна, в результате чего нельзя однозначно говорить о преимуществе байесовской векторной авторегрессии с переменными параметрами.

# Abstract
The main goal of this research was to compare forecast accuracy of Bayesian Vector Autoregression with time-varying parameters with forecasts of one-dimensional models and their combinations. ARIMA, exponential smoothing and Theta-method were used as one-dimensional opponents of TVP-BVAR. Two data sets were used to conduct this study. The first one consists of 9 monthly macroeconomic time series, while the second one is a set of daily price changes of 24 largest Russian companies. Forecasts for macroeconomic data set were computed for 3, 6, 12 and 24 months ahead, while for the second dataset with daily observations horizons from 7 to 260 days were used. I order to compare forecast accuracy of different models root mean square error for each model was divided by root mean square error of random walk model. This ratio was used as a main criterion for the comparison.
As a result of the conducted research, the following Outcomes were obtained: forecasts of TVP-BVAR were the most accurate in half cases for macroeconomic data and in more than one third of the cases for financial time series. Nevertheless, it worth mentioning that for financial data differences in relative error was not significant which means that it is hard to tell whether it has a significant advantage over one-dimensional models.

------

* Основной файл для сборки итоговой работы --- `main.rnw`
* Все составные части работы содержатся в директории `parts`
* Директория `parts/Data` содержит используемые данные
* Все расчеты и оценки моделей представлены в файле `parts/Models_fitting.R`



