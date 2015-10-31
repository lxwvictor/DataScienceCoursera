## Q3, Q6, Q8: Count the number of lines in each file.

wc -l *.json

## Q4: Print the 100th review.

head -100 yelp_academic_dataset_review.json | tail -1

## Q5: Find percentage of 5-star reviews.

echo \
`grep '"stars": 5' yelp_academic_dataset_review.json | wc -l` \
`wc -l yelp_academic_dataset_review.json` \
| awk '{print $1 / $2 * 100}'

## Q7: Find percentage of free Wi-Fi out of businesses with the
## attribute "Wi-Fi".

echo \
`grep Wi-Fi yelp_academic_dataset_business.json | wc -l` \
`grep '"Wi-Fi": "free"' yelp_academic_dataset_business.json | wc -l` \
| awk '{print $2 / $1 * 100}'

## Q9: Print the 1000th tip.

awk 'NR == 1000 {print}' *tip.json

## Q10: Find the user with > 10000 funny compliments.

grep -o '.*"compliments": {.*"funny": [0-9]*' *user.json \
| awk '$NF > 10000 {print $13 $NF}'

## Shuffle and split a file into multiple parts.

shuf yelp_academic_dataset_review.json \
| split -l 100000 -d --additional-suffix=.json - review-
