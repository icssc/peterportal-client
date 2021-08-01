import React, { FC } from 'react';

const ErrorPage: FC = () => {
    return (
        <div>
            <div style={{ display: 'flex', flexDirection: 'row' }}>
                <h1>404</h1>
                <h3>Page not found.</h3>
            </div>
        </div>
    )
}

export default ErrorPage;