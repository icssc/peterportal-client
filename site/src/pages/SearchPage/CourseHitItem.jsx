import React from "react";
import { get } from "lodash";
import CourseQuarterIndicator from "./CourseQuarterIndicator";


const CourseHitItem = (props) => (
  <div>
    <div style={{ display: "flex" }}>
      <div>
        <a href={"/course/" + props.result._id}>
          <h3>
            <span
              className={props.bemBlocks.item("id_department")}
              dangerouslySetInnerHTML={{
                __html: get(
                  props.result,
                  "highlight.id_department",
                  props.result._source.id_department
                ),
              }}
            ></span>
            &nbsp;
            <span
              className={props.bemBlocks.item("id_number")}
              dangerouslySetInnerHTML={{
                __html: get(
                  props.result,
                  "highlight.id_number",
                  props.result._source.id_number
                ),
              }}
            ></span>
            &nbsp;
            <span
              className={props.bemBlocks.item("name")}
              dangerouslySetInnerHTML={{
                __html: get(
                  props.result,
                  "highlight.name",
                  props.result._source.name
                ),
              }}
            ></span>
          </h3>
        </a>
      </div>

      <CourseQuarterIndicator terms={props.result._source.terms}/>
    </div>

    <div>
      <h4 className={"course-department_unit"}>
        {props.result._source.department}&nbsp;･&nbsp;
        {props.result._source.units[0]} units
      </h4>
      <p
        className={props.bemBlocks.item("description")}
        dangerouslySetInnerHTML={{
          __html: get(
            props.result,
            "highlight.description",
            props.result._source.description
          ),
        }}
      ></p>
      {props.result._source.prerequisite !== "" && (
        <p>
          <b>Prerequisite: </b> {props.result._source.prerequisite}
        </p>
      )}

      <p className={"course-department_unit"}>
        {props.result._source.ge_string}
      </p>

      <br />
    </div>
  </div>
);

export default CourseHitItem;
