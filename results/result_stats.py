import json
from sklearn.metrics import confusion_matrix

FILE = "results-4-4-2021-16:30:27.json"
jdat = json.loads(open(FILE, 'r').read())

num_ex = len(jdat)

# Precision.
total_count = 0
pred_not_unknown = 0
pred_not_unknown_correct = 0
for idx in jdat.keys():
  ex = jdat[idx]
  total_count += 1
  if ex['predicted'] != 'unknown':
    pred_not_unknown += 1
    if ex['success'] == True:
      pred_not_unknown_correct += 1

print("Precision: {}/{}, {}%".format(
  pred_not_unknown_correct,
  pred_not_unknown,
  pred_not_unknown_correct/pred_not_unknown,
))

y_true = [e['expected'] for k, e in jdat.items()]
y_pred = [e['predicted'] for k, e in jdat.items()]
print(confusion_matrix(y_true, y_pred, labels=['yes', 'no', 'unknown']))

