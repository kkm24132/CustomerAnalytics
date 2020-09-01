# Customer Analytics
Customer Analytics related solutions using ML approaches

## Key Stages of Roadmap:

**Customer Acquisition** --> **Customer Development** --> **Customer Retention**

![Customer Analytics Journey](/images/CustomerAnalyticsJourney.png)

## Key KPI areas:

- Campaign Performance
- Customer Life Time Value
- Attrition and Loyalty Management
- Cross sell and Product Holding
- Transaction Behaviour Analysis
- Service Request Analysis

# ITOps Analytics

Use Case Area           | Description                              | Remarks                                          | Algorithm / Technique |
------------------------|------------------------------------------|--------------------------------------------------|-----------------------|
Monitoring and Alerting | Predict future incidents and outages and generates alerts | Ex: Predicting service downtime, Spam classification, Incident categorization, Predict usage monitoring | Random Forest, XGBoost, LightGBM |
Security and Compliance | Look for spams, breaches and violations and prevent them from progressing | Detect anomalies in a volatile timeseries forecasting | LSTM, HTM |
Problem Resolution      | Focus on narrowing the problem and identify root causes and solutions quickly | Use Case: Incident Root Cause Analysis <br> Column Sample: Id, CPU_Load, Memory_Load, Network_Delays, Error_1001, Error_1002, Root_Cause <br> Target: Root_Cause (Multi-class) > Memory, Network_Delay, DB_Issue  | [Classification - Deep Learning model with Keras](https://nbviewer.jupyter.org/github/kkm24132/CustomerAnalytics_ITOpsAnalytics/blob/master/src/Incident_RCA.ipynb)  |
Capacity Planning       | Focus on predicting future capacity requirements as part of the planning function | Ex: Service load forecasting, Predict capacity needs | ARIMA, LSTM |
Helpdesk use cases      | Provide automated help to users for problems they face in using the systems and services | Ex: Self service helpdesk helps in answering questions from knowledge base | [Recommend IT Service HelpDesk - using NLP](https://github.com/kkm24132/CustomerAnalytics_ITOpsAnalytics/blob/master/src/Recommend_HelpDesk.ipynb) |  
