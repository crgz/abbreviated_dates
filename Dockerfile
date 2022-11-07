ARG  BASE_IMAGE
FROM ${BASE_IMAGE}
RUN apt update
RUN apt install -y git make
RUN groupadd parser \
    --gid 1201 \
    && useradd parser \
    --create-home \
    --gid 1201 \
    --shell /bin/bash \
    --uid 1200 \
    && usermod -a -G sudo parser \
    && echo 'ALL ALL = (ALL) NOPASSWD: ALL' >> /etc/sudoers \
    && echo 'parser:secret' | chpasswd
ENV HOME=/home/parser

USER parser
COPY prolog/ /app
ARG PORT
ENV PORT=$PORT
CMD swipl app/epigrapher.pl --port=${PORT} --no-fork --debug='http(request)'
EXPOSE ${PORT}