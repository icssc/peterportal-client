import ProfessorPage from '../ProfessorPage';

export async function generateMetadata({ params }: { params: Promise<{ id: string }> }) {
  const id = decodeURIComponent((await params).id);

  const input = encodeURIComponent(JSON.stringify({ ucinetid: id }));

  const url = `${process.env.BACKEND_ROOT_URL}/trpc/professors.get?input=${input}`;

  const res = await fetch(url);
  const json = await res.json();

  const professor = json.result.data;

  const title = professor.name;
  const description = `${professor.title} in ${professor.department}`;

  return {
    title,
    description,

    openGraph: {
      title,
      description,
      url: `https://peterportal.org/professor/${id}`,
      type: 'website',
    },

    twitter: {
      title,
      description,
      card: 'summary',
    },
  };
}

interface ProfessorPageParams {
  params: Promise<{ id: string }>;
}
const Page = async ({ params }: ProfessorPageParams) => {
  return <ProfessorPage ucinetid={decodeURIComponent((await params).id)} />;
};
export default Page;
