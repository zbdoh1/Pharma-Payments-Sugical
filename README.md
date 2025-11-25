# Pharma-Payments-by-College 

**Supplementary Methods**

Data on payments to health professionals from November 2019 to October 2024 were downloaded from the centralised Medicines Australia repository. (1) These reports include individual payment records with fields for company name, practitioner name, healthcare professional type, practice address, service type, event type, payment recipient, registration fees, travel costs, fees for service, and reporting period. These data were merged with legacy reports (October 2015–October 2019) obtained from the University of Sydney repository. (2)

To assign each doctor to a specialty, a manual search of the AHPRA Register of Practitioners was performed. (3) When an exact match was unclear, for example, for common names shared by multiple clinicians, additional verification was undertaken using hospital or clinic websites, conference speaker biographies, and other publicly available sources. Some practitioners held more than one specialty; to avoid double-counting, each individual was assigned a single primary specialty, determined through targeted online searches. Further details on database construction are available elsewhere. (4)

Each clinician’s primary specialty was then mapped to the relevant specialist college using the list of approved programs of study from AHPRA. (5) For example, a practitioner registered as a Specialist in Anaesthesia was assigned to the Australian and New Zealand College of Anaesthetists. The full list of specialties and corresponding colleges is provided in Table S1. Some specialties require dual fellowship; for the purposes of this analysis, the following allocations were applied:
Haematology - RACP
Microbiology - RACP
Immunology/Allergy - RACP

For each specialty group, we estimated pharmaceutical company reach (the proportion of clinicians who received ≥1 payment) by linking payment data to AHPRA 2023/24 specialist workforce counts. (5) The number of registered specialists at the end of the study period was used as the denominator, providing a conservative estimate of payment reach.
Formal data analysis was undertaken by M.F. and Z.D. using R version 4.5.0, with independent checking by T.L. Visualisation was completed by Z.D. The released analysis dataset excludes practitioner address fields to protect privacy, as some individuals listed personal rather than professional addresses.

**References:**
1. Transparency Reporting – Medicines Australia [Internet]. Disclosureaustralia.com.au. 2022 [cited 2025 Nov 19]. Available from: https://www.disclosureaustralia.com.au/search/

2. Bero L, Mintzes B, Grundy Q, Fabbri A, Williams J, Parker L. Pharmaceutical industry payments to healthcare professionals (May 2016-Apr 2017). University of Sydney.  2019 Mar 29 [cited 2025 Nov 19]; Available from: https://ses.library.usyd.edu.au/handle/2123/20217

3. ‌Australian Health Practitioner Regulation Agency. Australian Health Practitioner Regulation Agency - Register of practitioners [Internet]. Ahpra.gov.au. 2018. Available from: https://www.ahpra.gov.au/Registration/Registers-of-Practitioners.aspx

4. Forbes M, Harvey K, Schien S, Hooimeyer A, Pokorny A. Drug company payments to Australian healthcare professionals. medRxiv. 2025 Sep 10.  doi:10.1101/2025.09.10.25335461.

5. ‌Australian Health Practitioner Regulation Agency - Approved programs of study [Internet]. Ahpra.gov.au. 2025 [cited 2025 Nov 19]. Available from: https://www.ahpra.gov.au/Accreditation/Approved-Programs-of-Study.aspx?ref=Medical%20Practitioner&type=Specialist&_gl=1

6. ‌‌Australian Health Practitioner Regulation Agency - Annual Report 2023/24 Index of Tables - Table R9: Health practitioners with specialties. [Internet]. Ahpra.gov.au. 2025 [cited 2025 Nov 19]. Available from: https://www.ahpra.gov.au/Publications/Annual-reports/Annual-report-2024/Index-of-tables.aspx

