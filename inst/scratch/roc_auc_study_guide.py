from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import inch
from reportlab.lib import colors
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle, HRFlowable
)
from reportlab.graphics.shapes import Drawing, Line, Rect, String, Circle, PolyLine
from reportlab.graphics.charts.lineplots import LinePlot
from reportlab.graphics import renderPDF

OUTPUT = r"C:\Users\noliv\OneDrive\Documents\ctn0094.oliven\inst\scratch\roc_auc_study_guide.pdf"

doc = SimpleDocTemplate(
    OUTPUT,
    pagesize=letter,
    leftMargin=0.85*inch,
    rightMargin=0.85*inch,
    topMargin=0.85*inch,
    bottomMargin=0.85*inch,
)

styles = getSampleStyleSheet()

title_style = ParagraphStyle(
    "Title2",
    parent=styles["Title"],
    fontSize=20,
    spaceAfter=6,
)
h1 = ParagraphStyle("H1", parent=styles["Heading1"], fontSize=13, spaceAfter=4, spaceBefore=14)
h2 = ParagraphStyle("H2", parent=styles["Heading2"], fontSize=11, spaceAfter=3, spaceBefore=10)
body = ParagraphStyle("Body", parent=styles["Normal"], fontSize=10, leading=15, spaceAfter=6)
mono = ParagraphStyle("Mono", parent=styles["Code"], fontSize=9, leading=13,
                      backColor=colors.HexColor("#f4f4f4"), leftIndent=12, spaceAfter=6)
note = ParagraphStyle("Note", parent=styles["Normal"], fontSize=9, leading=13,
                      textColor=colors.HexColor("#555555"), leftIndent=12, spaceAfter=6)

def roc_drawing():
    """Draw a clean ROC curve with annotated points."""
    d = Drawing(340, 260)

    # Axes background
    d.add(Rect(50, 30, 240, 200, fillColor=colors.HexColor("#fafafa"),
               strokeColor=colors.HexColor("#cccccc"), strokeWidth=0.5))

    # Diagonal (random)
    d.add(Line(50, 30, 290, 230, strokeColor=colors.HexColor("#aaaaaa"),
               strokeWidth=1, strokeDashArray=[4, 3]))

    # ROC curve points (x = 1-spec, y = sens) — a realistic moderate curve
    pts = [
        (0.00, 0.00),
        (0.02, 0.10),
        (0.05, 0.22),
        (0.08, 0.31),
        (0.12, 0.44),
        (0.18, 0.55),
        (0.25, 0.64),
        (0.35, 0.72),
        (0.48, 0.80),
        (0.62, 0.87),
        (0.78, 0.93),
        (0.90, 0.96),
        (1.00, 1.00),
    ]

    def tx(x): return 50 + x * 240
    def ty(y): return 30 + y * 200

    # Draw ROC line segments
    for i in range(len(pts) - 1):
        x0, y0 = pts[i]
        x1, y1 = pts[i+1]
        d.add(Line(tx(x0), ty(y0), tx(x1), ty(y1),
                   strokeColor=colors.black, strokeWidth=1.8))

    # Blue dot — low threshold (high sens, high 1-spec)  ~0.78, 0.93
    bx, by = 0.78, 0.93
    d.add(Circle(tx(bx), ty(by), 6, fillColor=colors.HexColor("#2563eb"),
                 strokeColor=colors.white, strokeWidth=1))
    d.add(String(tx(bx) + 8, ty(by) + 4, "Low threshold",
                 fontSize=8, fillColor=colors.HexColor("#2563eb")))
    d.add(String(tx(bx) + 8, ty(by) - 6, "(flag everyone)",
                 fontSize=7.5, fillColor=colors.HexColor("#2563eb")))

    # Red dot — high threshold (low sens, low 1-spec) ~0.08, 0.31
    rx, ry = 0.08, 0.31
    d.add(Circle(tx(rx), ty(ry), 6, fillColor=colors.HexColor("#dc2626"),
                 strokeColor=colors.white, strokeWidth=1))
    d.add(String(tx(rx) + 8, ty(ry) + 4, "High threshold",
                 fontSize=8, fillColor=colors.HexColor("#dc2626")))
    d.add(String(tx(rx) + 8, ty(ry) - 6, "(flag very few)",
                 fontSize=7.5, fillColor=colors.HexColor("#dc2626")))

    # Axis labels
    d.add(String(155, 8, "1 - Specificity", fontSize=9, fillColor=colors.black))
    # Rotated "Sensitivity" label approximated with positioned text
    for i, ch in enumerate("Sensitivity"):
        d.add(String(10, 130 + (i - 5) * 9, ch, fontSize=9, fillColor=colors.black))

    # Tick marks and labels on x axis
    for v in [0.0, 0.25, 0.50, 0.75, 1.0]:
        xp = tx(v)
        d.add(Line(xp, 28, xp, 32, strokeColor=colors.black, strokeWidth=0.8))
        d.add(String(xp - 7, 18, f"{v:.2f}", fontSize=7.5, fillColor=colors.black))

    # Tick marks and labels on y axis
    for v in [0.0, 0.25, 0.50, 0.75, 1.0]:
        yp = ty(v)
        d.add(Line(48, yp, 52, yp, strokeColor=colors.black, strokeWidth=0.8))
        d.add(String(28, yp - 3, f"{v:.2f}", fontSize=7.5, fillColor=colors.black))

    return d

story = []

# ── Title ────────────────────────────────────────────────────────────────────
story.append(Paragraph("ROC AUC Study Guide", title_style))
story.append(Paragraph("OUD Relapse Prediction Context", styles["Heading2"]))
story.append(HRFlowable(width="100%", thickness=1, color=colors.HexColor("#333333")))
story.append(Spacer(1, 10))

# ── Setup ────────────────────────────────────────────────────────────────────
story.append(Paragraph("The Setup", h1))
story.append(Paragraph(
    "We have a logistic regression model predicting whether a patient with OUD will "
    "relapse after treatment. The outcome is binary:",
    body))
story.append(Paragraph("1 = relapsed &nbsp;&nbsp;&nbsp; 0 = stayed clean", mono))
story.append(Paragraph(
    "The model does not output a hard 0/1 — it outputs a <b>probability</b> for each patient: "
    "e.g., 0.73, 0.21, 0.55. To make a prediction you choose a <b>threshold</b>. "
    "Default is 0.5: above = predict relapse, below = predict clean.",
    body))

# ── 2x2 table ────────────────────────────────────────────────────────────────
story.append(Paragraph("The 2×2 Confusion Matrix", h1))

table_data = [
    ["", "Predicted Relapse (1)", "Predicted Clean (0)"],
    ["Actually Relapsed (1)", "True Positive (TP)", "False Negative (FN)"],
    ["Actually Clean (0)",    "False Positive (FP)", "True Negative (TN)"],
]
t = Table(table_data, colWidths=[1.7*inch, 1.9*inch, 1.9*inch])
t.setStyle(TableStyle([
    ("BACKGROUND",   (0, 0), (-1, 0), colors.HexColor("#1e3a5f")),
    ("TEXTCOLOR",    (0, 0), (-1, 0), colors.white),
    ("BACKGROUND",   (0, 1), (0, -1), colors.HexColor("#1e3a5f")),
    ("TEXTCOLOR",    (0, 1), (0, -1), colors.white),
    ("BACKGROUND",   (1, 1), (1, 1), colors.HexColor("#d1fae5")),  # TP green
    ("BACKGROUND",   (2, 2), (2, 2), colors.HexColor("#d1fae5")),  # TN green
    ("BACKGROUND",   (2, 1), (2, 1), colors.HexColor("#fee2e2")),  # FN red
    ("BACKGROUND",   (1, 2), (1, 2), colors.HexColor("#fee2e2")),  # FP red
    ("ALIGN",        (0, 0), (-1, -1), "CENTER"),
    ("VALIGN",       (0, 0), (-1, -1), "MIDDLE"),
    ("FONTNAME",     (0, 0), (-1, 0), "Helvetica-Bold"),
    ("FONTNAME",     (0, 1), (0, -1), "Helvetica-Bold"),
    ("FONTSIZE",     (0, 0), (-1, -1), 9),
    ("ROWBACKGROUNDS", (1, 1), (-1, -1), [colors.white, colors.HexColor("#f8f8f8")]),
    ("GRID",         (0, 0), (-1, -1), 0.5, colors.HexColor("#cccccc")),
    ("ROWHEIGHT",    (0, 0), (-1, -1), 28),
]))
story.append(t)
story.append(Spacer(1, 8))

# ── Sens / Spec ───────────────────────────────────────────────────────────────
story.append(Paragraph("Sensitivity and Specificity", h1))

story.append(Paragraph("<b>Sensitivity</b> — of everyone who actually relapsed, how many did we catch?", body))
story.append(Paragraph("TP / (TP + FN)", mono))
story.append(Paragraph(
    "Miss a relapser = False Negative. Low sensitivity = lots of missed relapses. "
    "In OUD, missing a relapse is costly — that patient doesn't get extra intervention.",
    note))

story.append(Paragraph("<b>Specificity</b> — of everyone who stayed clean, how many did we correctly leave alone?", body))
story.append(Paragraph("TN / (TN + FP)", mono))
story.append(Paragraph(
    "Wrongly flag a clean patient = False Positive. Low specificity = unnecessary alarms.",
    note))

# ── Threshold tradeoff ───────────────────────────────────────────────────────
story.append(Paragraph("The Threshold Tradeoff", h1))

thresh_data = [
    ["Threshold", "Who gets flagged", "Sensitivity", "Specificity"],
    ["Low (e.g. 0.3)",  "Almost everyone",  "High — catch most relapsers",  "Low — many false alarms"],
    ["High (e.g. 0.7)", "Very selective",   "Low — miss many relapsers",    "High — few false alarms"],
]
t2 = Table(thresh_data, colWidths=[1.1*inch, 1.4*inch, 2.0*inch, 2.0*inch])
t2.setStyle(TableStyle([
    ("BACKGROUND",  (0, 0), (-1, 0), colors.HexColor("#1e3a5f")),
    ("TEXTCOLOR",   (0, 0), (-1, 0), colors.white),
    ("FONTNAME",    (0, 0), (-1, 0), "Helvetica-Bold"),
    ("FONTSIZE",    (0, 0), (-1, -1), 8.5),
    ("ALIGN",       (0, 0), (-1, -1), "CENTER"),
    ("VALIGN",      (0, 0), (-1, -1), "MIDDLE"),
    ("ROWBACKGROUNDS", (1, 0), (-1, -1), [colors.HexColor("#eff6ff"), colors.HexColor("#fef2f2")]),
    ("GRID",        (0, 0), (-1, -1), 0.5, colors.HexColor("#cccccc")),
    ("ROWHEIGHT",   (0, 0), (-1, -1), 28),
]))
story.append(t2)
story.append(Spacer(1, 6))
story.append(Paragraph(
    "As threshold increases: sensitivity decreases monotonically, specificity increases monotonically.",
    note))

# ── ROC curve ────────────────────────────────────────────────────────────────
story.append(Paragraph("The ROC Curve", h1))
story.append(Paragraph(
    "Instead of picking one threshold, sweep it from 0 to 1 and plot every resulting "
    "(sensitivity, 1&minus;specificity) pair. Each point on the curve is one threshold value.",
    body))

story.append(roc_drawing())
story.append(Spacer(1, 4))

story.append(Paragraph(
    "<b>Blue dot</b> = low threshold: flag almost everyone. High sensitivity, low specificity. "
    "<b>Red dot</b> = high threshold: flag very few. Low sensitivity, high specificity. "
    "Moving along the curve from blue to red = increasing the threshold.",
    note))

# ── AUC ──────────────────────────────────────────────────────────────────────
story.append(Paragraph("AUC — Area Under the Curve", h1))
story.append(Paragraph(
    "AUC summarises the entire curve as one number — literally the area under it.",
    body))

auc_data = [
    ["AUC", "Meaning"],
    ["1.0", "Perfect — curve hugs the top-left corner"],
    ["0.75", "Good — meaningful discriminative ability"],
    ["0.5",  "Random — curve is the diagonal"],
    ["< 0.5","Worse than random — something is flipped"],
]
t3 = Table(auc_data, colWidths=[0.9*inch, 4.6*inch])
t3.setStyle(TableStyle([
    ("BACKGROUND",  (0, 0), (-1, 0), colors.HexColor("#1e3a5f")),
    ("TEXTCOLOR",   (0, 0), (-1, 0), colors.white),
    ("FONTNAME",    (0, 0), (-1, 0), "Helvetica-Bold"),
    ("FONTSIZE",    (0, 0), (-1, -1), 9),
    ("ALIGN",       (0, 0), (0, -1), "CENTER"),
    ("VALIGN",      (0, 0), (-1, -1), "MIDDLE"),
    ("ROWBACKGROUNDS", (1, 0), (-1, -1), [colors.white, colors.HexColor("#f8f8f8")]),
    ("GRID",        (0, 0), (-1, -1), 0.5, colors.HexColor("#cccccc")),
    ("ROWHEIGHT",   (0, 0), (-1, -1), 24),
]))
story.append(t3)
story.append(Spacer(1, 8))

# ── Picking the best threshold ────────────────────────────────────────────────
story.append(Paragraph("Picking the Best Threshold", h1))
story.append(Paragraph(
    'There is no single "best" threshold — it depends on clinical priorities. '
    "A common default is <b>Youden's J</b>: maximise sensitivity + specificity simultaneously.",
    body))
story.append(Paragraph("J = sensitivity + specificity - 1 &nbsp;&nbsp; (maximise this)", mono))
story.append(Paragraph(
    "For OUD relapse prediction, missing a relapser (false negative) is probably more costly "
    "than a false alarm. This argues for prioritising <b>sensitivity</b> — accepting a lower "
    "threshold even if it produces more false positives.",
    note))

# ── Tidymodels snippet ────────────────────────────────────────────────────────
story.append(Paragraph("In R (tidymodels)", h1))
story.append(Paragraph(
    "Find the threshold that maximises Youden's J from cross-validated predictions:", body))
story.append(Paragraph(
    "rs_results |&gt;<br/>"
    "&nbsp;&nbsp;collect_predictions() |&gt;<br/>"
    "&nbsp;&nbsp;roc_curve(truth = outcome, .pred_0, event_level = 'first') |&gt;<br/>"
    "&nbsp;&nbsp;mutate(j = sensitivity + specificity - 1) |&gt;<br/>"
    "&nbsp;&nbsp;slice_max(j, n = 1)",
    mono))
story.append(Paragraph(
    "Output: .threshold (the cutoff probability), sensitivity, specificity, j at that point.",
    note))

story.append(Spacer(1, 12))
story.append(HRFlowable(width="100%", thickness=0.5, color=colors.HexColor("#aaaaaa")))
story.append(Paragraph(
    "CTN-0094 OUD Prediction Project &mdash; Study Guide",
    ParagraphStyle("footer", parent=styles["Normal"], fontSize=8,
                   textColor=colors.HexColor("#888888"), alignment=1)))

doc.build(story)
print(f"Saved to {OUTPUT}")
