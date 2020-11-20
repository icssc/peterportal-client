import React from 'react'
import InfoHeader from '../../component/InfoHeader';
import axios from 'axios';
import AdditionalInfo from '../../component/AdditionalInfo';
import GradeDist from '../../component/GradeDist';
import PrereqTree from '../../component/PrereqTree';
import "./CoursePage.scss";

class CoursePage extends React.Component {
    constructor(props) {
        super(props);

        this.state = {
            courseData: null
        }
    }

    componentDidMount() {
        this.fetchCourseData()
    }

    fetchCourseData() {
        axios.get('/courses/api/' + this.props.match.params.id).then(res => this.setState({courseData: res.data}, () => console.log(this.state.courseData)))
    }


    render() {
        if (this.state.courseData != null) {
            return (
                <div style={{ display: "flex", flexDirection: "row" }}>
                    <div className="search-page">
                        <InfoHeader {...this.state.courseData}/>
                        <div className="course_page">
                            <AdditionalInfo {...this.state.courseData}/>
                            <PrereqTree {...this.state.courseData}/>
                            <GradeDist {...this.state.courseData}/>
                        </div>

                    </div>
                </div>
            )
        } else {
            return <h1>Loading...</h1>;
        }
    }
}

export default CoursePage;