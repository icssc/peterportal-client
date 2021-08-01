declare module "react-twemoji" {
    import React from 'react';

    export interface TwemojiProps {
        options?: Object;
        noWrapper?: boolean;
        tag?: string;
    }

    declare const MyComponent: React.FC<TwemojiProps>
    export default MyComponent
}