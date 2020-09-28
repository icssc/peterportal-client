import React from 'react'

class CoursePage extends React.Component {
    constructor(props) {
        super(props);

        this.state = {
            courseData: null,
            professorNames: {},
            dependenciesNames: {},
            prerequisiteNames: {},
        }
    }

    componentDidMount() {

    }

    render() {
        return (
            <>
                <div>hello world</div>
            </>
        )
    }
}

export default CoursePage;