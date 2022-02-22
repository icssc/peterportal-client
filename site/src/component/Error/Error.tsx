import React, { useState, useEffect, Component, FC } from 'react';
import './Error.scss';

interface ErrorProps {
    message: string;
}

const Error: FC<ErrorProps> = (props) => {
    return <div className='error'>
        <img src='/working.gif'></img>
        <h1>404 PAGE NOT FOUND</h1>
        <h2>{props.message}</h2>
    </div>
}

export default Error;