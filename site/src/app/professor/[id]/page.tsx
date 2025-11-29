import ProfessorPage from '../ProfessorPage';
import { createServerSideTrpcCaller } from '../../../trpc';
import { headers } from 'next/headers';

interface ProfessorPageParams {
  params: Promise<{ id: string }>;
}

export async function generateMetadata({ params }: ProfessorPageParams) {
  const id = decodeURIComponent((await params).id);

  const reqHeaders = await headers().then((h) => Object.fromEntries(h.entries()));
  const serverTrpc = createServerSideTrpcCaller(reqHeaders);
  const professor = await serverTrpc.professors.get.query({ ucinetid: id });

  const title = professor.name;
  const description = `${professor.title} in ${professor.department}`;

  return {
    title,
    description,
  };
}

const Page = async ({ params }: ProfessorPageParams) => {
  return <ProfessorPage ucinetid={decodeURIComponent((await params).id)} />;
};
export default Page;
