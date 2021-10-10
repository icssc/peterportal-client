import React from 'react';
import { Form } from 'react-bootstrap';
import './Filter.scss';

interface CustomFilterOptionProps {
    onClick: Function;
    label: string;
    count: number | string;
    active?: boolean;
}

const CustomFilterOption = (props: CustomFilterOptionProps) => {
    const label = <div className='filter-label'>
        <div className='filter-label-name'>{props.label}</div>
        <div className='filter-label-count'>{props.count}</div>
    </div>
    return (
        <div className='filter-option'>
            <Form.Check type='checkbox' id={`filter-checkbox-${props.label}`} checked={props.active} label={label} onClick={() => props.onClick()} />
        </div>
    )
}

export default CustomFilterOption;