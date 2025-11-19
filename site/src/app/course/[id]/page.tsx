import CoursePage from '../CoursePage';

export async function generateMetadata({ params }: { params: Promise<{ id: string }> }) {
  const id = decodeURIComponent((await params).id);

  const input = encodeURIComponent(JSON.stringify({ courseID: id }));

  const url = `${process.env.BACKEND_ROOT_URL}/trpc/courses.get?input=${input}`;

  const res = await fetch(url);
  const json = await res.json();

  const course = json.result.data;

  const title = `${course.department} ${course.courseNumber} | ${course.title}`;
  const description = course.description;

  return {
    title,
    description,

    openGraph: {
      title,
      description,
      url: `https://peterportal.org/course/${id}`,
      type: 'website',
    },

    twitter: {
      title,
      description,
      card: 'summary',
    },
  };
}

interface CoursePageParams {
  params: Promise<{ id: string }>;
}
const Page = async ({ params }: CoursePageParams) => {
  return <CoursePage courseId={decodeURIComponent((await params).id)} />;
};
export default Page;
