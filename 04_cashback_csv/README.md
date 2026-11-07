# Варіант 5. Кешбек-рахунок онлайн-банку

Окремий sbt-проєкт `04_cashback_csv` (n04, імпорт CSV). Попередній проєкт `03_cashback` не змінюється: там модель стану без журналу. Тут рядки CSV ідуть залізничним парсером (`Either`) і згортаються в стан кешбек-рахунку.

- slug: `bank_cashback_account`
- CSV: `testdata/variant_05_bank_cashback_account.csv` (500000 рядків)
- інваріант: `forbid_withdraw_below_minimum_threshold` (виведення менше 100 грн заборонене)

Коди подій: `ACCRUE`, `WITHDRAW`, `PARTNER_BONUS`, `REFUND_REVERSAL`. Формат рядка: `event,p1,p2,p3,p4`.

## Генерація журналу

З клону курсу `vplanto/functional_programming`:

```shell
cd n04_state_event_logs/scripts
python generate_event_logs.py --variant 5 --rows 500000
```

Очікуване ім’я: `variant_05_bank_cashback_account.csv`. Готовий файл лежить у `testdata/`. Фрагмент для тестів: `src/test/resources/variant_05_fragment.csv`.

## Запуск

```shell
cd 04_cashback_csv
sbt test
sbt "run testdata/variant_05_bank_cashback_account.csv"
```
