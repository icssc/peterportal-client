import React from 'react';
import { ResponsivePie } from '@nivo/pie';

const gradeScale = ["A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-"]
const gpaScale= [4.0, 3.7, 3.3, 3.0, 2.7, 2.3, 2.0, 1.7, 1.3, 1.0, 0,7]

export default class Pie extends React.Component {
    constructor(props) {
        super(props);

        this.total = 0;
        this.totalPNP = 0;
        this.averageGPA = 0;
        this.averageGrade = "";

        this.getClassData = this.getClassData.bind(this);
    }

    getClassData() {
        let gradeACount = 0, gradeBCount = 0, gradeCCount = 0, gradeDCount = 0,
          gradeFCount = 0, gradePCount = 0, gradeNPCount = 0;

        this.total = 0;
        this.totalPNP = 0;
        this.averageGPA = 0;
        this.averageGrade = "";

        var sum = 0
        

        this.props.gradeData.forEach(data => {
          if (data.AcadTerm === this.props.quarter
            && data.instructor === this.props.professor) {
                console.log(data);
              gradeACount += data.a_count;
              gradeBCount += data.b_count;
              gradeCCount += data.c_count;
              gradeDCount += data.d_count;
              gradeFCount += data.f_count;
              gradePCount += data.p_count;
              gradeNPCount += data.np_count;
              sum += ((4.0 * data.a_count) + (3.0 * data.b_count) + (2.0 * data.c_count) + (1.0 * data.d_count))
              this.total += data.a_count + data.b_count + data.c_count
              + data.d_count + data.f_count + data.p_count + data.np_count;
              this.totalPNP += data.p_count + data.np_count;
          }
        });

        console.log(this.total);
        console.log(this.totalPNP);
        this.averageGPA = (sum / (this.total - this.totalPNP)).toFixed(1)
        this.gpaToGradeConverter(this.averageGPA);

        return [
          {
            "id": "A",
            "label": "A",
            "value": gradeACount,
            "color": "#60A3D1"
          },
          {
            "id": "B",
            "label": "B",
            "value": gradeBCount,
            "color": "#81C284"
          },
          {
            "id": "C",
            "label": "C",
            "value": gradeCCount,
            "color": "#F5D77F"
          },
          {
            "id": "D",
            "label": "D",
            "value": gradeDCount,
            "color": "#ECAD6D"
          },
          {
            "id": "F",
            "label": "F",
            "value": gradeFCount,
            "color": "#E8966D"
          }
        //   {
        //     "id": "P",
        //     "label": "P",
        //     "value": gradePCount,
        //     "color": "#4AB486"
        //   },
        //   {
        //     "id": "NP",
        //     "label": "NP",
        //     "value": gradeNPCount,
        //     "color": "#E36436"
        //   },
        ];
      }

    gpaToGradeConverter(gpa) {
      for(var i = 0; gpa < gpaScale[i]; i++){}
      this.averageGrade = gradeScale[i];
    }

    render() {
        return(
            <div style={{width: "100%"}}>
            <ResponsivePie
                data={this.getClassData()}
                margin={{
                    top: 50,
                    bottom: 50,
                    left: 15,
                    right: 15
                  }}
                enableRadialLabels={false}
                innerRadius={0.8}
                padAngle={2}
                colors={["#60A3D1", "#81C284", "#F5D77F", "#ECAD6D","#E8966D"]}
                cornerRadius={3}
                borderWidth={1}
                borderColor={{ from: 'color', modifiers: [ [ 'darker', 0.2 ] ] }}
                enableSlicesLabels={false}
                tooltip={({ id, value, color }) => (
                    <strong style={{ color }}>
                        {id}: {(value/(this.total-this.totalPNP) * 100).toFixed(2)}%
                    </strong>
                )}
            />
            <div style={{display: "flex",  textAlign: "center", margin: "-235px"}}>
                  <div style={{margin: "auto"}}>
                    <h3>Average Grade: {this.averageGrade} ({this.averageGPA})</h3>
                    <h3 style={{marginBottom: "6px"}}>Total Enrolled: <strong>{this.total}</strong></h3>
                    { this.totalPNP > 0 ? <small>{this.totalPNP} enrolled as P/NP</small> : null}
                
                </div>
            </div>
            </div>
            
        )
    }
}

