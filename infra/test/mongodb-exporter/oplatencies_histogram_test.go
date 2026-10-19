package exporter

import (
	"strings"
	"testing"

	"go.mongodb.org/mongo-driver/bson"
	"go.mongodb.org/mongo-driver/bson/primitive"
)

// MongoDB 8.3 reports serverStatus.opLatencies.<kind>.histogram as an array of
// {micros, count} buckets. 0.51.0 flattens every bucket to one name and label set,
// so each scrape logged 64 "was collected before with the same name and label values"
// errors (~187k journal lines a day on mongo-1) and served a meaningless 0.
func TestOpLatenciesHistogramIsSkippedLikeHistograms(t *testing.T) {
	status := bson.M{"opLatencies": bson.M{"reads": bson.M{
		"latency": int64(1000),
		"ops":     int64(10),
		"histogram": primitive.A{
			bson.M{"micros": int64(8), "count": int64(3)},
			bson.M{"micros": int64(64), "count": int64(7)},
		},
	}}}

	var names []string
	for _, metric := range makeMetrics("ss", status, map[string]string{}, true) {
		names = append(names, metric.Desc().String())
	}
	all := strings.Join(names, "\n")

	if strings.Contains(all, "histogram") {
		t.Errorf("a histogram series was emitted:\n%s", all)
	}
	for _, kept := range []string{"opLatencies_latency", "opLatencies_ops"} {
		if !strings.Contains(all, kept) {
			t.Errorf("%s was not emitted:\n%s", kept, all)
		}
	}
}
