import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import nrel.routee.powertrain as pt

# to do:
# reverse route
# don't go over speed limit

elevation = {
	"dia": 5434,
	"denver": 5280,
	"arvada": 5525,
	"golden": 5784,
	"idaho_springs": 7526,
	"georgetown": 8521,
	"silverthorne": 8790,
	"copper": 9712,
	"vail": 8239
	}

# point A, point B, distance [miles], speed limit [mph], actual speed [mph]
legs = {
	"leg1": ["dia", "denver", 20, 65, 67],
	"leg2": ["denver", "arvada", 5, 60, 52],
	"leg3": ["arvada", "golden", 8, 60, 57],
	"leg4": ["golden", "idaho_springs", 21, 65, 61],
	"leg5": ["idaho_springs", "georgetown", 22, 70, 76],
	"leg6": ["georgetown", "silverthorne", 22, 70, 73],
	"leg7": ["silverthorne", "copper", 11, 65, 62],
	"leg8": ["copper", "vail", 19, 55, 51]
	}

# need to do return trip here, before df creation

def calculate_grade(a_elev, b_elev, dist):

	grade = 100 * (b_elev - a_elev) / dist

	return grade

for i in legs:
	g_i = calculate_grade(
		a_elev = elevation[legs[i][0]],
		b_elev = elevation[legs[i][1]],
		dist = legs[i][2] * 5280
		)
	legs[i].append(g_i)

legs_df = pd.DataFrame.from_dict(
	legs,
	orient = "index",
	columns = ["POINT_A", "POINT_B", "distance", "speed_limit", "speed_mph", "grade_percent"]
	).reset_index(drop = True)

# legs_df["ELEV_A"] = legs_df["POINT_A"].map(elevation)

# legs_return_df = legs_df.iloc[::-1]
# legs_return_df.rename(columns = {"POINT_A": "POINT_B", "POINT_B": "POINT_A"}, inplace = True)
# # legs_return_df.drop(columns = ["ELEV_A"], inplace = True)
# legs_return_df["grade_percent"] = -legs_return_df["grade_percent"]
# legs_df = pd.concat([legs_df, legs_return_df]).reset_index(drop = True)

camry = pt.load_model("2016_TOYOTA_Camry_4cyl_2WD")

legs_df["TIME_MINS"] = legs_df["distance"] * 60 / legs_df["speed_mph"]

legs_df["DISTANCE_ALONG"] = legs_df["distance"].cumsum()

legs_df["GGE_ACTUAL"] = camry.predict(
	legs_df,
	feature_columns = ["speed_mph", "grade_percent"],
	apply_real_world_adjustment = True
	)

speed_adj = 10

worst_gge = []
limit_gge = []
optimal_gge = []
worst_speed = []
optimal_speed = []
for i in legs:

	speed_min = legs[i][3] - speed_adj
	speed_max = legs[i][3] + speed_adj

	speed_range = {
    	"speed_mph": {"lower": speed_min, "upper": speed_max, "n_samples": speed_adj * 2 + 1},
    	"grade_percent": {"lower": legs[i][5], "upper": legs[i][5], "n_samples": 1}
		}
	range_gge = pt.visualize_features(
		camry,
		speed_range,
		output_path = "./viz/speed_range_" + i,
		return_predictions = True
		)

	# print(range_gge["speed_mph"])

	optimal_speed_i = range_gge["speed_mph"].idxmin()
	# optimal_speed_i = range_gge["speed_mph"][range_gge["speed_mph"] == range_gge["speed_mph"].min()]
	worst_speed_i = range_gge["speed_mph"].idxmax()
	# print(optimal_speed_i.index.max())

	optimal_gge_per100 = range_gge["speed_mph"][optimal_speed_i]
	if optimal_gge_per100 == range_gge["speed_mph"][legs[i][3]]:
		optimal_speed_i = legs[i][3]
	# elif optimal_speed_i.index.max() < legs[i][3]:


	worst_gge_per100 = range_gge["speed_mph"][worst_speed_i]

	worst_speed.append(worst_speed_i)
	optimal_speed.append(optimal_speed_i)

	optimal_gge_leg = optimal_gge_per100 * (legs[i][2] / 100)
	worst_gge_leg = worst_gge_per100 * (legs[i][2] / 100)

	df_worst = pd.DataFrame({
		"distance": [legs[i][2]],
		"speed_mph": [worst_speed_i],
		"grade_percent": [legs[i][5]]
		})
	df_optimal = pd.DataFrame({
		"distance": [legs[i][2]],
		"speed_mph": [optimal_speed_i],
		"grade_percent": [legs[i][5]]
		})
	df_limit = pd.DataFrame({
		"distance": [legs[i][2]],
		"speed_mph": [legs[i][3]],
		"grade_percent": [legs[i][5]]
		})

	pred_worst = camry.predict(
		df_worst,
		feature_columns = ["speed_mph", "grade_percent"],
		apply_real_world_adjustment = True
		)
	pred_limit = camry.predict(
		df_limit,
		feature_columns = ["speed_mph", "grade_percent"],
		apply_real_world_adjustment = True
		)
	pred_optimal = camry.predict(
		df_optimal,
		feature_columns = ["speed_mph", "grade_percent"],
		apply_real_world_adjustment = True
		)

	worst_gge.append(pred_worst.loc[0, "gge"])
	limit_gge.append(pred_limit.loc[0, "gge"])
	optimal_gge.append(pred_optimal.loc[0, "gge"])

legs_df["SPEED_WORST"] = worst_speed
legs_df["SPEED_OPTIMAL"] = optimal_speed

legs_df["GGE_WORST"] = worst_gge
legs_df["GGE_LIMIT"] = limit_gge
legs_df["GGE_OPTIMAL"] = optimal_gge

legs_df["GGE_ALONG_ACTUAL"] = legs_df["GGE_ACTUAL"].cumsum()
legs_df["GGE_ALONG_WORST"] = legs_df["GGE_WORST"].cumsum()
legs_df["GGE_ALONG_LIMIT"] = legs_df["GGE_LIMIT"].cumsum()
legs_df["GGE_ALONG_OPTIMAL"] = legs_df["GGE_OPTIMAL"].cumsum()

point_df = pd.DataFrame.from_dict(elevation, orient = "index", columns = ["y"]).reset_index(names = "point")
point_df = pd.merge(point_df, legs_df[["POINT_B", "DISTANCE_ALONG"]], how = "left", left_on = "point", right_on = "POINT_B")
point_df.drop(columns = ["POINT_B"], inplace = True)
point_df.rename(columns = {"DISTANCE_ALONG": "x"}, inplace = True)
point_df["x"].fillna(value = 0, limit = 1, inplace = True)

# plt.plot(point_df["x"], point_df["y"])
# plt.xlabel("Miles Travelled")
# plt.ylabel("Elevation [ft]")
# plt.show()

plt.plot(legs_df["DISTANCE_ALONG"], legs_df["GGE_ALONG_ACTUAL"],
	color = "black", label = "Actual")
# plt.plot(legs_df["DISTANCE_ALONG"], legs_df["GGE_ALONG_LIMIT"],
# 	color = "black", linestyle = "dashed", label = "Speed Limit")
plt.plot(legs_df["DISTANCE_ALONG"], legs_df["GGE_ALONG_WORST"],
	color = "red", label = "Worst Case")
plt.plot(legs_df["DISTANCE_ALONG"], legs_df["GGE_ALONG_OPTIMAL"],
	color = "blue", label = "Optimal")
plt.xlabel("Miles Travelled")
plt.ylabel("Gasoline Used")
plt.legend(title = "Usage Scenario")
plt.savefig("./viz/gge_along.png")
plt.show()

gge_limit = legs_df["GGE_ALONG_LIMIT"].max()
gge_actual = legs_df["GGE_ALONG_ACTUAL"].max()
gge_worst = legs_df["GGE_ALONG_WORST"].max()
gge_optimal = legs_df["GGE_ALONG_OPTIMAL"].max()

eff_score = 1 - (gge_actual - gge_optimal) / (gge_worst - gge_optimal)

trip_summary = {
	"time_mins": legs_df["TIME_MINS"].sum(),
	"gge": legs_df["GGE_ALONG_ACTUAL"].max(),
	"distance_miles": legs_df["distance"].sum(),
	"mpg": legs_df["distance"].sum() / legs_df["GGE_ALONG_ACTUAL"].max(),
	"eff": eff_score
	}

print("----- Trip Summary --------------------------------------------")
print("Distance:", trip_summary["distance_miles"], "miles")
print("Time:",  int(trip_summary["time_mins"] // 60), "hrs",
	int(trip_summary["time_mins"] % 60), "mins")
print("Gallons of Gasoline:", round(trip_summary["gge"], 4))
print("MPG:", round(trip_summary["mpg"], 1))
print("Efficiency:", round(trip_summary["eff"], 3))
print("---------------------------------------------------------------")

# print(legs_df[["speed_limit", "GGE_LIMIT", "speed_mph", "GGE_ACTUAL",
# 	"SPEED_WORST", "GGE_WORST", "SPEED_OPTIMAL", "GGE_OPTIMAL"]])