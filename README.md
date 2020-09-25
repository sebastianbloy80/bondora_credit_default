# bondora_credit_default

Disclaimer

The information contained in this document and possible resources available for download are not intended as, and
shall not be understood or construed as, financial advise. I am not an attorney, accountant or financial advisor, nor
am I holding myself out to be. This document and the information contained within does not substitute financial advice from professionals.
The data used in this analysis has been taken from an external source. I cannot and will not guarantee the correctness of said data, nor can or will I guarantee the correctness of any conclusion drawn from it. Furthermore all used
statistical methods must not represent the optimal course of analysis and may be subject to later changes.
Use all information given in this document at your own discretion and risk.

1. Motivation

1.1  Target and motivation of the analysis
It is the target of this analysis to try and find a statistical model from data provided by Bondora itself which enables the prediction of the probability of a loan to be defaulting or not defaulting for the entirety of its duration. This prediction shall result in a binary outcome. The analysis is motivated by pure personal interest in the matter.

1.2  Origin of Data
Bondora Capital OÜ1 is providing a web-plattform for so called peer-to-peer creditory services. Private investors can buy into credits taken by private persons from Estonia, Finland, Slovakia and Spain. The credit itself is given out by a Bank. Bondora provides the platform to partake in those credits as a private investor.

The whole credit itself is not part of the offer but it is split up into several piece of differing amount.

If the investor decides to invest into loans by his own selection, Bondora itself provides a credit grading system, ranging from „AA“ (safest grade) up to „HR“ (highest risk). There is also a „Credit Score“ Value which ranging from „0“ to „1000“ whereas the higher the number indicates a more positive payment behavior of the borrower than can be assumed with lower numbers2. 

This analysis does not try to criticize Bondora's credit grading system in any way. It merely uses the data kindly provided on Bondora's web-page freely to be analyzed.
