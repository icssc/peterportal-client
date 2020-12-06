import React from 'react';
import { min, max } from 'lodash';

export default function CourseSideInfo(props) {
    return (
        <div>
            <div style={{display: "flex", backgroundColor: "#EEEEEE", padding: "1rem"}}>
                <div>
                    💻
                </div>
                <div style={{marginLeft: "1.5rem"}}>
                    <h2 style={{marginBottom: "0.2rem"}}>{props.id}</h2>
                    <h5 style={{margin: 0}}>{props.title}</h5>
                </div>
            </div>

            <div style={{padding: "1.5rem"}}>
                <p>{props.school}&nbsp;･&nbsp;
                    {min(props.units) === max(props.units) ? 
                        <span>{props.units[0]} units</span> : 
                        <span>{min(props.units)} - {max(props.units)} units</span>}
                </p>
                
                <h5>Course Description</h5>
                <p>{props.description}</p>
                        
                {props.ge_list.length != 0 && <div>
                    <h5>GE Criteria</h5>
                        {props.ge_list.map((e) => 
                        <div>
                            <span>
                                {e}
                            </span>
                        </div>
                        )}<br/>
                    </div>
                }

                {props.professor_history.length != 0 && <div>
                    <h5>Instructor History</h5>
                        <p>{props.professor_history.map((e) => 
                            <div>
                                <span>
                                    {e}
                                </span>
                            </div>
                        )}</p><br/>
                    </div>
                }
            </div>
        </div>
    )
}