import React from 'react';
import {Grid, Divider} from "semantic-ui-react";
import './InfoHeader.scss';

function InfoHeader(props) {
    return (
        <div id="course_main-header">
            <Grid.Row className="course_info-container">
            <Grid.Column width={2} style={{display: "flex", marginRight: "2em"}} >
                <span style={{display: "flex", fontSize: "46px", margin: "auto"}}><span role="img" aria-label="laptop">💻</span></span>
            </Grid.Column>
            <Grid.Column width={10}>
                <h1 id="course_id">{props.id}<span id="course_name">{props.title}</span></h1>
                <p id="course_dept-school-unit">

                {props.school}&nbsp;･&nbsp;<span>{props.department}
                &nbsp;･&nbsp;{props.units[0]} units</span>
                </p>
            </Grid.Column>
            </Grid.Row>
        </div> 
    );
}

export default InfoHeader;