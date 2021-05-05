import React from 'react';

export default function ProfessorPage(props) {
    return (
        <div>
            <div style={{display: "flex", flexDirection: "row"}}>
                <h1>Professor {props.match.params.id}</h1>
            </div>
        </div>
    )
}