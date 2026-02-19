#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.12"
# dependencies = ["pyarrow"]
# ///

import pyarrow as pa
import pyarrow.parquet as pq
from datetime import date
from pathlib import Path

table1 = pa.table({
    "id": pa.array([1, 2, 3, 4], type=pa.int32()),
    "name": pa.array(["alice", "bob", "carol", "dave"], type=pa.utf8()),
    "score": pa.array([97.5, 82.3, 91.0, 76.8], type=pa.float64()),
    "passed": pa.array([True, True, True, False], type=pa.bool_()),
    "exam_date": pa.array(
        [date(2025, 1, 10), date(2025, 1, 11), date(2025, 1, 10), date(2025, 1, 12)],
        type=pa.date32(),
    ),
})

table2 = pa.table({
    "id": pa.array([5, 6, 7, 8], type=pa.int32()),
    "name": pa.array(["erika", "francis", "genevieve", "hugo"], type=pa.utf8()),
    "score": pa.array([96.5, 72.3, 81.0, 76.8], type=pa.float64()),
    "passed": pa.array([True, True, True, False], type=pa.bool_()),
    "exam_date": pa.array(
        [date(2025, 1, 10), date(2025, 1, 11), date(2025, 1, 10), date(2025, 1, 12)],
        type=pa.date32(),
    ),
})

out1 = Path(__file__).parent / "test1.parquet"
pq.write_table(table1, out1)
print(f"Wrote {out1} ({out1.stat().st_size} bytes, {table1.num_rows} rows)")

out2 = Path(__file__).parent / "test2.parquet"
pq.write_table(table2, out2)
print(f"Wrote {out2} ({out2.stat().st_size} bytes, {table2.num_rows} rows)")
