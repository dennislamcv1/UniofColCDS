-- Data Quality Dimensions (C2M4V3)

-- EXAMPLE: Length of Stay calculation using date_diff
select date_diff(visit_end_date, visit_start_date,day)
from mimic3_demo_omop_corrected.visit_occurrence
limit 10;

-- Length of Stay DQ min/max/avg
SELECT MIN(date_diff(visit_end_date, visit_start_date,day)) as MIN_LOS,
       MAX(date_diff(visit_end_date, visit_start_date,day)) as MAX_LOS,
       AVG(date_diff(visit_end_date, visit_start_date,day)) as AVG_LOS
from mimic3_demo_omop_corrected.visit_occurrence


-- Histogram of condition_concept_ids -- NO LABELS
SELECT condition_concept_id, 
             count(condition_concept_id) as NUM_DX
FROM mimic3_demo_omop_corrected.condition_occurrence
GROUP BY condition_concept_id
ORDER BY NUM_DX desc

-- Histogram of condition_concept_ids --  LABELS
SELECT co.condition_concept_id, c.concept_id, c.concept_name,
             count(condition_concept_id) as NUM_DX
FROM mimic3_demo_omop_corrected.condition_occurrence co
   JOIN mimic3_demo_omop_corrected.concept c 
          on (co.condition_concept_id = c.concept_id)
GROUP BY co.condition_concept_id, c.concept_id, c.concept_name
ORDER BY NUM_DX desc

-- Events per year
SELECT EXTRACT(year from visit_start_date) as HOSP_YR,
      count(visit_occurrence_id) as NUM_VISITS
from mimic3_demo_omop_corrected.visit_occurrence
group by HOSP_YR
order by HOSP_YR asc
