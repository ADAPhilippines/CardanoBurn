import type { NextPage } from 'next';
import Head from 'next/head';
import Image from 'next/image';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faGithub } from '@fortawesome/free-brands-svg-icons';

const Home: NextPage = () => {
  return (
    <div className="bg-main text-white h-[100vh] bg-effect bg-cover bg-top">

      <Head>
        <title>Cardano Burn 🔥</title>
        <meta name="description" content="Burn your $ADA 🔥" />
        <link rel="icon" href="/favicon.ico" />
      </Head>

      <header className="flex p-6 relative border-solid border-b-2 border-brdr-fade">
        <div className="w-1/4 h-[60px] relative">
          <Image src="/cardano_burn.svg" alt="Cardano Burn" layout="fill"/>
        </div>
        <div className="absolute right-[calc(50%-237px/2)] top-[calc(50%-24px/2)]">🔥 Burn your $ADA for the lulz 🔥</div>
        <div className="w-full" />
        <nav className='flex items-center space-x-12'>
          <a href="#" className="font-bold tracking-widest">ABOUT</a>
          <a href="#" className="text-[32px]">
            <FontAwesomeIcon icon={faGithub} />
          </a>
        </nav>
      </header>
    </div>
  );
}

export default Home;
